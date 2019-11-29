defmodule Effusion.BTP.DownloadServer do
  use GenServer, restart: :transient
  alias Effusion.Application.DownloadServerSupervisor
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.BTP.Download
  alias Effusion.BTP.Metainfo.Directory
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.OutgoingHandler
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
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

  def connected(info_hash, peer_id, address) when is_hash(info_hash) and is_peer_id(peer_id) do
    GenServer.cast(
      {:via, Registry, {SessionRegistry, info_hash}},
      {:connected, peer_id, address}
    )
  end


  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %Download{}, from, {:piece, block})
       when is_peer_id(from) do

     cancel_messages = Request.cancel(block, from)
       |> Enum.map(fn {peer_id, index, offset, size} ->
         {:btp_send, peer_id, {:cancel, index, offset, size}}
       end)
       |> Enum.uniq()


     d = Map.update!(d, :pieces, &Pieces.add_block(&1, block))
     verified = Pieces.verified(d.pieces)

     have_messages =
       verified
       |> Enum.map(&{:broadcast, {:have, &1.index}})

     write_messages =
       d.pieces
       |> Pieces.verified()
       |> Enum.map(fn p -> {:write_piece, d.info_hash, d.file, p} end)

    peer_request_query = from peer_piece in PeerPiece,
                          join: peer in assoc(peer_piece, :peer),
                          where: peer.peer_id == ^from
    peer_request_count = Repo.aggregate(peer_request_query, :count, :peer_id)

    {d, request_messages} = if peer_request_count <= 0 do
      next_request_from_peer(d, from, Application.get_env(:effusion, :max_requests_per_peer))
    else
      {d, []}
    end

    {d, write_messages ++ have_messages ++ cancel_messages ++ request_messages}
  end

  def handle_message(d = %Download{}, remote_peer_id, {:bitfield, bitfield}) when is_peer_id(remote_peer_id) and is_binary(bitfield) do
    peer = Repo.one!(from p in Peer, where: [peer_id: ^remote_peer_id])
    indicies = IntSet.new(bitfield) |> Enum.to_list()
    pieces_query = Piece.all_indicies_query(d.info_hash, indicies)
    pieces = Repo.all(pieces_query)
    peer_pieces = Enum.map(pieces, fn p ->
      %{
        peer_id: peer.id,
        piece_id: p.id
      }
    end)

    Repo.insert_all(PeerPiece, peer_pieces)

    interest_message =
      if Pieces.has_pieces?(d.pieces, bitfield) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {d, interest_message}
  end

  def handle_message(d = %Download{}, remote_peer_id, {:have, i}) do
    peer = from p in Peer, where: p.peer_id == ^remote_peer_id
    piece = from p in Piece,
             join: torrent in assoc(p, :torrent),
             where: torrent.info_hash == ^d.info_hash
                and p.index == ^i

    peer = Repo.one!(peer)
    piece = Repo.one!(piece)

    Repo.insert(%PeerPiece{
      peer: peer,
      piece: piece
    })

    interest_message =
      if Pieces.has_piece?(d.pieces, i) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {d, interest_message}
  end

  def handle_message(d = %Download{}, remote_peer_id, :choke) do
    Repo.delete_all(from request in Request,
                      join: peer in assoc(request, :peer),
                      where: peer.peer_id == ^remote_peer_id)
    {d, []}
  end

  def handle_message(d = %Download{}, remote_peer_id, :unchoke) do
    {d, request_messages} = next_request_from_peer(d, remote_peer_id, 100)

    {d, request_messages}
  end

  def handle_message(d = %Download{}, _remote_peer_id, _msg), do: {:ok, d, []}

  defp next_request_from_peer(d, peer_id, count) do
    info_hash = d.info_hash

    requests = Request.valid_requests_from_peer_query(info_hash, peer_id, count)
    |> Repo.all()

    requests_to_insert = Enum.map(requests, fn {_piece, block, peer} ->
      %{
        block_id: block.id,
        peer_id: peer.id
      }
    end)
    Repo.insert_all(Request, requests_to_insert)

    request_messages = Enum.map(requests, fn {piece, block, peer} ->
      {:btp_send, peer.peer_id, {:request, piece.index, block.offset, block.size}}
    end)

    {d, request_messages}
  end



  defp handle_internal_message({:btp_connect, address, local_info_hash, local_peer_id, expected_peer_id}, state = %Download{})
  when is_hash(local_info_hash)
   and is_peer_id(local_peer_id)
   and is_peer_id(expected_peer_id) or is_nil(expected_peer_id) do
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

  defp handle_internal_message({:write_piece, info_hash, destdir, block}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    Effusion.IOServer.write_piece(info_hash, destdir, block)
    state = Download.mark_piece_written(state, block.index)
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":write_piece latency: #{stop - start} μs")
    state
  end

  defp handle_internal_message({:announce, announce_params}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)
    {:ok, res} = apply(@thp_client, :announce, announce_params)

    messages = handle_tracker_response(state, res)
    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":write_piece latency: #{stop - start} μs")
    state
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start_download(session) do
    _ = Logger.info("Starting download #{Effusion.Hash.inspect(session.info_hash)}")

    Repo.delete_all(PeerPiece)
    Repo.delete_all(Request)

    session = Map.put(session, :started_at, Timex.now())
    params = announce_params(session, :started)

    messages = [{:announce, params}]
    {session, messages}
  end

  defp announce_params(d, event) do
    {local_host, local_port} = d.local_address

    tracker_numwant = Application.get_env(:effusion, :tracker_numwant)
    opts = [event: event, numwant: tracker_numwant]

    opts = case d.trackerid do
      "" -> opts
      _str -> opts |> Keyword.merge([trackerid: d.trackerid])
    end

    [
      d.announce,
      local_host,
      local_port,
      d.peer_id,
      d.info_hash,
      0,
      Pieces.bytes_completed(d.pieces),
      Pieces.bytes_left(d.pieces),
      opts
    ]
  end

  defp handle_tracker_response(d, res) do
    trackerid =
      if Map.get(res, :trackerid, "") != "" do
        res.trackerid
      else
        d.trackerid
      end

    changesets = Enum.map(res.peers, fn peer ->
      %{
        address: %Postgrex.INET{address: peer.ip},
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)

    Repo.insert_all(Peer, changesets, on_conflict: :nothing)

    d =
      d
      |> Map.put(:trackerid, trackerid)

    max_peers = Application.get_env(:effusion, :max_peers)
    eligible_peers = PeerSelection.select_lowest_failcount(max_peers)

    Enum.map(eligible_peers, fn p ->
      connect_message(p.address.address, p.port, d.info_hash, d.peer_id, p.peer_id)
    end)
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

    case handle_message(state, peer_id, msg) do
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
    Peer.insert(peer_id, address)
    stop = System.monotonic_time(:microsecond)
    Logger.debug(":connected latency: #{stop - start} μs")
    {:noreply, state}
  end

  def handle_cast({:unregister_connection, {ip, port}, reason}, state = %Download{}) do
    start = System.monotonic_time(:microsecond)

    messages = PeerSelection.select_lowest_failcount(1)
        |> Enum.map(fn peer ->
          connect_message(
            peer.address.address,
            peer.port,
            state.info_hash,
            state.peer_id,
            peer.peer_id)
        end)

    Repo.one(from peer in Peer,
              where: peer.address == ^%Postgrex.INET{address: ip}
              and peer.port == ^port)
    |> Peer.changeset()
    |> Repo.update(update: [inc: [failcount: 1]])

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

    stop = System.monotonic_time(:microsecond)
    Logger.debug(":unregister_connection latency: #{stop - start} μs")

    {:noreply, state}
  end

  defp connect_message(address, port, info_hash, local_peer_id, remote_peer_id)
    when is_hash(info_hash)
     and is_peer_id(local_peer_id)
     and is_peer_id(remote_peer_id) or is_nil(remote_peer_id) do
    {
      :btp_connect,
      {address, port},
      info_hash,
      local_peer_id,
      remote_peer_id
    }
  end

  def handle_info(:timeout, state = %Download{}) do
    {state, messages} = start_download(state)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

    {:noreply, state}
  end

  def handle_info(:interval_expired, state = %Download{}) do
    announce_params = announce_params(state, :interval)
    {:ok, res} = apply(@thp_client, :announce, announce_params)

    messages = handle_tracker_response(state, res)
    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    state = Enum.reduce(messages, state, &handle_internal_message(&1, &2))

    {:noreply, state}
  end

  def handle_info(_, state = %Download{}) do
    {:noreply, state}
  end

  def terminate(:normal, state = %Download{}) do
    ConnectionRegistry.disconnect_all(state.meta.info_hash)

    announce_params =
      if Download.done?(state) do
        announce_params(state, :completed)
      else
        announce_params(state, :stopped)
      end

    {:ok, _res} = apply(@thp_client, :announce, announce_params)

    reply_to_listeners(state, {:ok, Download.pieces(state)})
  end

  def terminate(reason, state = %Download{}) do
    Logger.debug("download server terminating with reason: #{inspect(reason)}")

    announce_params = announce_params(state, :stopped)
    {:ok, _res} = apply(@thp_client, :announce, announce_params)

    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state = %Download{}, msg) do
    Download.each_listener(state, fn l -> GenServer.reply(l, msg) end)
  end
end
