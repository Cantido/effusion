defmodule Effusion.BTP.DownloadServer do
  use GenServer, restart: :transient
  require Logger
  alias Effusion.BTP.Download
  alias Effusion.PWP.ConnectionRegistry

  @moduledoc """
  An API to manage a `Effusion.BTP.Download` object as it is connected to many peers simultaneously.
  """

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  @doc """
  Start the Download Server in its own supervision tree.
  """
  def start(meta, {_host, _port} = local_server, file) when is_map(meta) do
    Effusion.Application.DownloadServerSupervisor.start_child([meta, local_server, file])
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

  @doc """
  Wait on a download managed by a session server to complete.
  """
  def await(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :await, :infinity)
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(info_hash, peer_id, message) do
    GenServer.call(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:handle_msg, peer_id, message}
    )
  end

  @doc """
  Handle a peer disconnection.
  """
  def unregister_connection(info_hash, peer_id, address \\ nil) do
    GenServer.cast(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:unregister_connection, peer_id, address}
    )
  end

  def connected(info_hash, peer_id, address) do
    GenServer.cast(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:connected, peer_id, address}
    )
  end

  defp handle_internal_message({:btp_connect, peer}, state = %Download{}) do
    Effusion.PWP.Connection.connect(peer)
    state
  end

  defp handle_internal_message({:broadcast, outgoing_msg}, state = %Download{}) do
    ConnectionRegistry.btp_broadcast(state.meta.info_hash, outgoing_msg)
    state
  end

  defp handle_internal_message({:btp_send, remote_peer_id, outgoing_msg}, state = %Download{}) do
    ConnectionRegistry.btp_send(state.meta.info_hash, remote_peer_id, outgoing_msg)
    state
  end

  defp handle_internal_message({:write_piece, info, destdir, block}, state = %Download{}) do
    Effusion.IO.write_piece(info, destdir, block)
    Download.mark_piece_written(state, block.index)
  end

  ## Callbacks

  def init([meta, local_peer, file]) do
    state = Download.new(meta, local_peer, file)

    {:ok, state, 0}
  end

  def handle_call({:handle_msg, peer_id, msg}, _from, state = %Download{}) do
    _ = Logger.debug("Got a message!!! #{inspect(msg)}")

    case Download.handle_message(state, peer_id, msg) do
      {:error, reason} ->
        _ = Logger.error "Download encountered error: #{inspect reason}"
        {:stop, reason, {:error, reason}, state}

      {state, messages} ->
        _ = Logger.debug("replying: #{inspect(messages)}")

        state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

        if Download.done?(state) do
          {:stop, :normal, :ok, state}
        else
          {:reply, :ok, state}
        end
    end
  end

  def handle_call(:await, from, state = %Download{}) do
    state = Download.add_listener(state, from)
    {:noreply, state}
  end

  def handle_cast({:connected, peer_id, address}, state = %Download{}) do
    {:noreply, Download.handle_connect(state, peer_id, address)}
  end

  def handle_cast({:unregister_connection, peer_id, address}, state = %Download{}) do
    {session, messages} = Download.handle_disconnect(state, peer_id, address)

    Enum.each(messages, &handle_internal_message(&1, state))

    {:noreply, session}
  end

  def handle_info(:timeout, state = %Download{}) do
    {session, announce_response, messages} = Download.start(state, @thp_client)

    Process.send_after(self(), :interval_expired, announce_response.interval * 1_000)

    Enum.each(messages, &handle_internal_message(&1, state))

    {:noreply, session}
  end

  def handle_info(:interval_expired, state = %Download{}) do
    {session, res} = Download.announce(state, @thp_client)

    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    {:noreply, session}
  end

  def handle_info(_, state = %Download{}) do
    {:noreply, state}
  end

  def terminate(:normal, state = %Download{}) do
    ConnectionRegistry.disconnect_all(state.meta.info_hash)

    {state, _response} =
      if Download.done?(state) do
        Download.announce(state, @thp_client, :completed)
      else
        Download.announce(state, @thp_client, :stopped)
      end

    reply_to_listeners(state, {:ok, Download.pieces(state)})
  end

  def terminate(reason, state = %Download{}) do
    Logger.debug "download server terminating with reason: #{inspect reason}"

    {state, _res} = Download.announce(state, @thp_client, :stopped)
    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state = %Download{}, msg) do
    Download.each_listener(state, fn l -> GenServer.reply(l, msg) end)
  end
end
