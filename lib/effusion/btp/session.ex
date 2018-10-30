defmodule Effusion.BTP.Session do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  import Effusion.BTP.Peer
  require Logger

  @moduledoc """
  The top-level data structure of a downloading file.

  This module decides when to connect to peers, when to ask for pieces,
  when to write pieces to disk, etc.
  """

  @local_peer_id Application.get_env(:effusion, :peer_id)

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
      peers: Map.new(),
      peer_addresses: Map.new(),
      connected_peers: Map.new(),
      closed_connections: MapSet.new(),
      peer_id: @local_peer_id,
      listeners: MapSet.new(),
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
    {s, cancel_messages} = cancel_block_requests(s, block, from)

    torrent = Torrent.add_block(s.torrent, block)
    verified = Torrent.verified(torrent)

    have_messages = verified
    |> Enum.map(&({:broadcast, {:have, &1.index}}))

    {:ok, torrent} = Effusion.IO.write_to(torrent, s.file)

    {
      %{s | torrent: torrent},
      have_messages ++ cancel_messages
    }
  end

  defp cancel_block_requests(s, block, from) do
    block_id = Block.id(block)

    messages =
      s
      |> Map.get(:requested_pieces, MapSet.new())
      |> Map.get(block_id, MapSet.new())
      |> Enum.filter(&(&1 != from))
      |> Enum.map(&({&1, {:cancel, block_id}}))

    Map.update(s, :requested_pieces, Map.new(), &Map.delete(&1, block_id))
    {s, messages}
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
    next_block =
      Effusion.BTP.PieceSelection.next_block(
        s.torrent,
        Map.values(s.peers),
        # actual_connected_peers,
        @block_size
      )

    case next_block do
      nil -> {nil, s}
      _ ->
        s1 = Map.update!(s, :requested_pieces, &MapSet.put(&1, next_block))
        {next_block, s1}
    end
  end

  defp next_request_msg(session) do
    case next_request(session) do
      {{peer_id, %{index: i, offset: o, size: sz}}, session} -> {session, [{peer_id, {:request, i, o, sz}}]}
      {nil, session} -> {session, []}
    end
  end

  @doc """
  Announce an event to this download's tracker,
  using the given Tracker HTTP Protocol (THP) client.
  """
  def announce(%{} = s, client, event \\ :interval) do
    {local_host, local_port} = Map.get(s, :local_address)

    {:ok, res} =
      client.announce(
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

    known_addrs = Map.keys(s.peers)
    {known_ids, _} = Map.split(s.peer_addresses, known_addrs)

    new_peers =
      res.peers
      |> Enum.map(fn p ->
        Peer.new({p.ip, p.port}, s.peer_id, s.meta.info_hash, self())
        |> Peer.set_remote_peer_id(Map.get(p, :peer_id, nil))
      end)
      |> Enum.reject(&Enum.member?(known_ids, &1.remote_peer_id))
      |> Enum.reject(&Enum.member?(known_addrs, &1.address))
      |> Map.new(&({&1.address, &1}))

    announced_addrs = res.peers
    |> Enum.map(&({&1.ip, &1.port}))
    |> MapSet.new()

    all_peers = Map.merge(new_peers, s.peers)
    {dec_failcount, keep_failcount} = Map.split(all_peers, announced_addrs)

    all_updated_peers = dec_failcount
    |> Enum.map(fn {addr, p} -> {addr, Peer.dec_fail_count(p)} end)
    |> Map.new()
    |> Map.merge(keep_failcount)

    peer_addresses = all_updated_peers
    |> Enum.reject(fn {_addr, p} -> p.remote_peer_id == nil end)
    |> Map.new(fn {addr, p} -> {p.remote_peer_id, addr} end)

    tracker_id =
      if Map.get(res, :tracker_id, "") != "" do
        res.tracker_id
      else
        s.tracker_id
      end


    s = s
    |> Map.put(:peers, all_updated_peers)
    |> Map.put(:peer_addresses, peer_addresses)
    |> Map.put(:tracker_id, tracker_id)

    {s, res}
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session, thp_client) do
    {session, response} = announce(session, thp_client, :started)
    session = connect_to_all(session)
    {session, response}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(s = %{peer_id: peer_id}, remote_peer_id, msg)
      when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    case session_handle_message(s, remote_peer_id, msg) do
      {:error, reason} ->
        {:error, reason}

      {s, session_messages} ->
        {s, peer_messages} = delegate_message(s, remote_peer_id, msg)
        {s, session_messages ++ peer_messages}
    end
  end

  defp session_handle_message(s, _remote_peer_id, {:bitfield, b}) do
    pieces_count = Enum.count(s.meta.info.pieces)
    max_i = Enum.max(IntSet.new(b), fn -> 0 end)

    if max_i in 0..(pieces_count - 1) do
      {s, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(s, _remote_peer_id, {:have, i}) do
    pieces_count = Enum.count(s.meta.info.pieces)

    if i in 0..(pieces_count - 1) do
      {s, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(s, remote_peer_id, {:piece, b}) do
    {s, block_messages} = add_block(s, b, remote_peer_id)
    {s, request_messages} = next_request_msg(s)
    {s, block_messages ++ request_messages}
  end

  defp session_handle_message(s, _remote_peer_id, :unchoke) do
    next_request_msg(s)
  end

  defp session_handle_message(s, _remote_peer_id, _msg), do: {s, []}

  defp delegate_message(s, remote_peer_id, msg)
       when is_peer_id(remote_peer_id) do

    # select peer
    {:ok, peer} = get_connected_peer(s, remote_peer_id)

    {peer, responses} = Peer.recv(peer, msg)

    session = Map.update(s, :peers, Map.new(), &Map.put(&1, peer.address, peer))
    {session, Enum.map(responses, fn r -> {remote_peer_id, r} end)}
  end

  defp get_connected_peer(s, remote_peer_id)
       when is_peer_id(remote_peer_id) do

    {:ok, address} = Map.fetch(s.peer_addresses, remote_peer_id)
    Map.fetch(s.peers, address)
  end

  defp peer(s, peer_id, peer_address)
       when is_peer_id(peer_id) do
    Peer.new(peer_address, s.peer_id, s.meta.info_hash, self())
    |> Map.put(:remote_peer_id, peer_id)
  end

  def handle_connect(s, peer_id, address)
      when is_peer_id(peer_id) do
    _ = Logger.debug("Handling connection success to #{inspect(address)}")
    s
    |> Map.update(:peers, Map.new(), &Map.put(&1, address, peer(s, peer_id, address)))
    |> Map.update(:peer_addresses, Map.new(), &Map.put(&1, peer_id, address))
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(s, peer_id, address)
      when is_peer_id(peer_id) do
    s
    |> Map.update(:peers, Map.new(), &Map.delete(&1, address))
    |> increment_connections()
  end

  defp increment_connections(s) do
    selected = Effusion.BTP.PeerSelection.select_peer(
      s.peer_id,
      s.peers,
      []
    )

    case selected do
      nil -> s
      peer -> connect_to_peer(s, peer)
    end
  end

  defp connect_to_all(s) do
    s.peers
    |> Enum.reduce(s, fn {_addr, p}, session -> connect_to_peer(session, p) end)
  end

  defp connect_to_peer(s, peer) do
    {:ok, _} = Effusion.PWP.Connection.connect(peer)
    s
  end
end
