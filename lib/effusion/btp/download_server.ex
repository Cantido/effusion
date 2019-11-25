defmodule Effusion.BTP.DownloadServer do
  use GenServer, restart: :transient
  alias Effusion.Application.DownloadServerSupervisor
  alias Effusion.BTP.Download
  alias Effusion.BTP.Metainfo.Directory
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.OutgoingHandler
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  require Logger

  @moduledoc """
  An API to manage a `Effusion.BTP.Download` object as it is connected to many peers simultaneously.
  """

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  @doc """
  Start the Download Server in its own supervision tree.
  """
  def start(meta, {_host, _port} = local_server, file) when is_map(meta) do
    case DownloadServerSupervisor.start_child([meta, local_server, file]) do
      {:ok, _pid} -> {:ok, meta.info_hash}
      err -> err
    end
  end

  @doc """
  Start the session server and link it to the current process.
  """
  def start_link([meta, local_peer, file]) do
    GenServer.start_link(
      __MODULE__,
      [meta, local_peer, file],
      name: {:via, Registry, {SessionRegistry, meta.info_hash}}
    )
  end

  def get(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :get, 10_000)
  end

  @doc """
  Wait on a download managed by a session server to complete.
  """
  def await(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :await, :infinity)
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(info_hash, peer_id, message) when is_hash(info_hash) and is_peer_id(peer_id) do
    GenServer.call(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:handle_msg, peer_id, message}
    )
  end

  @doc """
  Handle a peer disconnection.
  """
  def unregister_connection(info_hash, address, reason) do
    GenServer.cast(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:unregister_connection, address, reason}
    )
  end

  def connected(info_hash, peer_id, address) do
    GenServer.cast(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:connected, peer_id, address}
    )
  end

  defp handle_internal_message({:btp_connect, address, local_info_hash, local_peer_id, expected_peer_id}, state = %Download{})
  when is_hash(local_info_hash)
   and is_peer_id(local_peer_id)
   and is_peer_id(expected_peer_id) do
    start = System.monotonic_time(:microsecond)
    OutgoingHandler.connect({address, local_info_hash, local_peer_id, expected_peer_id})
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":btp_connect latency: #{stop - start} μs")

    state
  end

  defp handle_internal_message({:broadcast, outgoing_msg}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    ConnectionRegistry.btp_broadcast(state.meta.info_hash, outgoing_msg)
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":broadcast latency: #{stop - start} μs")
    state
  end

  defp handle_internal_message({:btp_send, remote_peer_id, outgoing_msg}, state = %Download{})
  when is_peer_id(remote_peer_id)
   and not is_nil(outgoing_msg) do
    start = System.monotonic_time(:microsecond)
    ConnectionRegistry.btp_send(state.meta.info_hash, remote_peer_id, outgoing_msg)
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":btp_send latency: #{stop - start} μs")
    state
  end

  defp handle_internal_message({:write_piece, info, destdir, block}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    Effusion.IOServer.write_piece(info, destdir, block)
    state = Download.mark_piece_written(state, block.index)
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":write_piece latency: #{stop - start} μs")
    state
  end

  defp handle_internal_message({:announce, announce_params}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    {:ok, res} = apply(@thp_client, :announce, announce_params)

    {state, messages} = Download.handle_tracker_response(state, res)
    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":write_piece latency: #{stop - start} μs")
    state
  end

  ## Callbacks

  def init([meta, local_peer, file]) do
    :ok = Directory.insert(meta)
    state = Download.new(meta, local_peer, file)

    {:ok, state, 0}
  end

  def handle_call({:handle_msg, peer_id, msg}, _from, state = %Download{}) when is_peer_id(peer_id) do
    handle_msg_start = System.monotonic_time(:microsecond)
    _ = Logger.debug("DownloadServer handling message from #{peer_id}: #{inspect(msg)}")

    case Download.handle_message(state, peer_id, msg) do
      {:error, reason} ->
        _ = Logger.error("Download encountered error: #{inspect(reason)}")
        {:stop, reason, {:error, reason}, state}

      {state, messages} ->
        download_handle_message_stop = System.monotonic_time(:microsecond)
        Logger.debug("Download.handle_message #{inspect msg} latency: #{download_handle_message_stop - handle_msg_start} μs")

        state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

        if Download.done?(state) do
          handle_msg_stop = System.monotonic_time(:microsecond)
          Logger.debug(":handle_msg #{inspect msg} (download done) latency: #{handle_msg_stop - handle_msg_start} μs")
          {:stop, :normal, :ok, state}
        else
          handle_msg_stop = System.monotonic_time(:microsecond)
          Logger.debug(":handle_msg #{inspect msg} (download not done) latency : #{handle_msg_stop - handle_msg_start} μs")
          {:reply, :ok, state}
        end
    end
  end

  def handle_call(:get, _from, state = %Download{}) do
    {:reply, state, state}
  end

  def handle_call(:await, from, state = %Download{}) do
    state = Download.add_listener(state, from)
    {:noreply, state}
  end

  def handle_cast({:connected, peer_id, address}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    reply = {:noreply, Download.handle_connect(state, peer_id, address)}
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":connected latency: #{stop - start} μs")
    reply
  end

  def handle_cast({:unregister_connection, address, reason}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    {state, messages} = Download.handle_disconnect(state, address, reason)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

    stop = System.monotonic_time(:microsecond)
    Logger.debug(":unregister_connection latency: #{stop - start} μs")

    {:noreply, state}
  end

  def handle_info(:timeout, state = %Download{}) do
    {state, messages} = Download.start(state)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

    {:noreply, state}
  end

  def handle_info(:interval_expired, state = %Download{}) do
    announce_params = Download.announce_params(state, :interval)
    {:ok, res} = apply(@thp_client, :announce, announce_params)
    state = Download.handle_tracker_response(state, res)

    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    {:noreply, state}
  end

  def handle_info(_, state = %Download{}) do
    {:noreply, state}
  end

  def terminate(:normal, state = %Download{}) do
    ConnectionRegistry.disconnect_all(state.meta.info_hash)

    announce_params =
      if Download.done?(state) do
        Download.announce_params(state, :completed)
      else
        Download.announce_params(state, :stopped)
      end

    {:ok, _res} = apply(@thp_client, :announce, announce_params)

    reply_to_listeners(state, {:ok, Download.pieces(state)})
  end

  def terminate(reason, state = %Download{}) do
    Logger.debug("download server terminating with reason: #{inspect(reason)}")

    announce_params = Download.announce_params(state, :stopped)
    {:ok, _res} = apply(@thp_client, :announce, announce_params)

    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state = %Download{}, msg) do
    Download.each_listener(state, fn l -> GenServer.reply(l, msg) end)
  end
end
