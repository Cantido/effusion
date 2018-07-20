defmodule Effusion.BTP.Session do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  alias Effusion.PWP.Connection
  import Effusion.BTP.Peer
  require Logger

  @moduledoc """
  The top-level data structure of a downloading file.

  This module decides when to connect to peers, when to ask for pieces,
  when to write pieces to disk, etc.
  """

  @local_peer_id "Effusion Experiment!"

  @block_size Application.get_env(:effusion, :block_size)

  @doc """
  Create a new download.

  This only creates a data structure. To actually start the download, call `start/1`.
  """
  def new(meta, local_address, file \\ nil) do
    %{
      file: file,
      meta: meta,
      torrent: Torrent.new(meta.info_hash),
      local_address: local_address,
      peers: MapSet.new(),
      connected_peers: Map.new(),
      closed_connections: MapSet.new(),
      peer_id: @local_peer_id,
      listeners: MapSet.new(),
      requested: MapSet.new(),
      requested_pieces: MapSet.new(),
      tracker_id: ""
    }
  end

  @doc """
  Get the blocks that have not been assembled into pieces and verified.
  """
  def blocks(s) do
    Torrent.unfinished(s.torrent)
  end

  @doc """
  Get the set of listeners waiting for this torrent to finish.
  """
  def listeners(s) do
    s.listeners
  end

  @doc """
  Get the torrent that this download is downloading.
  """
  def torrent(s) do
    s.torrent
  end

  @doc """
  Add a block of data to this download.

  This may trigger messages to be sent to any connections associated with this download's torrent.
  """
  def add_block(s, block, from) when is_peer_id(from) do
    s = cancel_block_requests(s, block, from)

    torrent = s.torrent

    pieces_before = Torrent.bitfield(torrent)
    torrent = Torrent.add_block(torrent, block)
    pieces_after = Torrent.bitfield(torrent)

    :ok = Effusion.PieceStage.put_event({s.meta.info_hash, s.meta.info, s.file, block})

    case IntSet.difference(pieces_after, pieces_before) |> Enum.to_list() do
      [new_piece] -> Connection.btp_broadcast(s.meta.info_hash, {:have, new_piece})
      _ -> :ok
    end

    %{s | torrent: torrent}
  end

  defp cancel_block_requests(s, block, from) do
    block_id = Block.id(block)
    peers_with_request = s
    |> Map.get(:requested_pieces, MapSet.new())
    |> Map.get(block_id, MapSet.new())

    Connection.btp_broadcast(s.meta.info_hash, {:cancel, block_id}, fn peer_id ->
      peer_id != from && MapSet.member?(peers_with_request, peer_id)
    end)

    Map.update(s, :requested_pieces, Map.new(), &Map.delete(&1, block_id))
  end

  @doc """
  Add a process that should be notified when this download completes or crashes.
  """
  def add_listener(s, from) do
    Map.update!(s, :listeners, &MapSet.put(&1, from))
  end

  @doc """
  Perform a function on all of this download's listening processes.
  """
  def each_listener(%{listeners: listeners}, fun)
  when is_function(fun, 1) do
    Enum.each(listeners, &fun.(&1))
  end

  @doc """
  Check if this download has received all necessary bytes.
  """
  def done?(s) do
    Torrent.all_present?(s.torrent)
  end

  @doc """
  Get the next piece that this download should ask for.
  """
  def next_request(s) do
    next_block = Effusion.BTP.PieceSelection.next_block(
      s.torrent,
      Map.values(s.connected_peers),
      @block_size)
    s1 = Map.update!(s, :requested, &MapSet.put(&1, next_block))

    {next_block, s1}
  end

  @doc """
  Announce an event to this download's tracker,
  using the given Tracker HTTP Protocol (THP) client.
  """
  def announce(s, client, event \\ :interval) do
    {local_host, local_port} = s.local_address

    {:ok, res} = client.announce(
      s.meta.announce,
      local_host,
      local_port,
      s.peer_id,
      s.meta.info_hash,
      0,
      Torrent.bytes_completed(s.torrent),
      Torrent.bytes_left(s.torrent),
      event,
      s.tracker_id
    )

    Process.send_after(self(), :interval_expired, res.interval * 1_000)

    peers = res.peers
    |> Enum.map(fn p ->
        Peer.new(
          {p.ip, p.port},
          s.peer_id,
          s.meta.info_hash,
          self())
        |> Peer.set_remote_peer_id(p.peer_id)
      end)

    tracker_id = if Map.get(res, :tracker_id, "") != "" do
      res.tracker_id
    else
      s.tracker_id
    end

    s
    |> Map.put(:peers, MapSet.new(peers))
    |> Map.put(:tracker_id, tracker_id)
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session, thp_client) do
    session
    |> announce(thp_client, :started)
    |> connect_to_all()
  end

  defp next_request_msg(session) do
    case next_request(session) do
      {{_peer_id, %{index: i, offset: o, size: sz}}, session} -> {session, [{:request, i, o, sz}]}
      {nil, session} -> {session, []}
    end
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(s = %{peer_id: peer_id}, remote_peer_id, msg)
  when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do

    case session_handle_message(s, remote_peer_id, msg) do
      {:error, reason} -> {:error, reason}
      {s, session_messages} ->
        {s, peer_messages} = delegate_message(s, remote_peer_id, msg)
        {s, session_messages ++ peer_messages}
    end
  end

  defp session_handle_message(s, _remote_peer_id, {:bitfield, b}) do
    pieces_count = Enum.count(s.meta.info.pieces)
    max_i = Enum.max(IntSet.new(b), fn -> 0 end)

    if max_i in 0..(pieces_count-1) do
      {s, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(s, _remote_peer_id, {:have, i}) do
    pieces_count = Enum.count(s.meta.info.pieces)

    if i in 0..(pieces_count-1) do
      {s, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(s, remote_peer_id, {:piece, b}) do
    s = add_block(s, b, remote_peer_id)
    next_request_msg(s)
  end

  defp session_handle_message(s, _remote_peer_id, :unchoke) do
    next_request_msg(s)
  end


  defp session_handle_message(s, _remote_peer_id, _msg), do: {s, []}

  defp delegate_message(s = %{peer_id: peer_id}, remote_peer_id, msg)
  when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    peer = get_connected_peer(s, remote_peer_id)
    {peer, responses} = Peer.recv(peer, msg)

    session = add_connected_peer(s, peer)
    {session, responses}
  end

  defp get_connected_peer(s = %{peer_id: peer_id}, remote_peer_id)
  when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    s
    |> Map.get(:connected_peers, Map.new())
    |> Map.get(remote_peer_id, default_peer(s, remote_peer_id))
  end

  defp peer(s, peer_id, peer_address)
  when is_peer_id(peer_id) do
    Peer.new(
      peer_address,
      s.peer_id,
      s.meta.info_hash,
      self())
    |> Map.put(:remote_peer_id, peer_id)
  end

  defp default_peer(%{peer_id: peer_id, meta: %{info_hash: info_hash}}, remote_peer_id)
  when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    Peer.new(
      {nil, nil},
      peer_id,
      info_hash,
      self())
    |> Peer.set_remote_peer_id(remote_peer_id)
  end

  @doc """
  Add a peer to this session's list of connected peers.

  This does not mean the session will connect to the peer,
  it is pretty much only used for testing.
  """
  def add_connected_peer(s = %{peer_id: peer_id}, peer = %{remote_peer_id: remote_peer_id})
  when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    Map.update!(s, :connected_peers, &Map.put(&1, remote_peer_id, peer))
  end

  @doc """
  Remove a peer to this session's list of connected peers.

  This does not mean the session will connect to the peer,
  it is pretty much only used for testing.
  """
  def remove_connected_peer(s, peer_id)
  when is_peer_id(peer_id) do
    Map.update!(s, :connected_peers, &Map.delete(&1, peer_id))
  end

  defp add_closed_connection(s, peer_id, address)
  when is_peer_id(peer_id) do
    Map.update!(s, :closed_connections, &MapSet.put(&1, peer(s, peer_id, address)))
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(s, peer_id, address)
  when is_peer_id(peer_id) do
    s
    |> remove_connected_peer(peer_id)
    |> add_closed_connection(peer_id, address)
    |> increment_connections()
  end

  defp increment_connections(s) do
    case select_peer(s) do
      nil -> s
      peer -> connect_to_peer(s, peer)
    end
  end

  defp connect_to_all(s) do
    s.peers
    |> Enum.reduce(s, fn p, session -> connect_to_peer(session, p) end)
  end

  defp select_peer(s) do
    disconnected_addresses = Enum.map(s.closed_connections, fn c -> c.address end) |> MapSet.new()
    disconnected_ids = Enum.map(s.closed_connections, fn c -> c.remote_peer_id end) |> MapSet.new()

    eligible_peers = s.peers
    |> Enum.reject(fn(p) ->
      peer_id = Map.get(p, :remote_peer_id)

      reject_peer_id? = peer_id != nil && ((peer_id == s.peer_id) || MapSet.member?(disconnected_ids, peer_id))
      reject_address? = MapSet.member?(disconnected_addresses, p.address)

      reject_address? || reject_peer_id?
    end)

    if Enum.empty?(eligible_peers) do
      nil
    else
      Enum.random(eligible_peers)
    end
  end

  defp connect_to_peer(s, peer) do
    {:ok, _} = Effusion.PWP.Connection.connect(peer)
    s
  end
end
