defmodule Effusion.BTP.Download do
  alias Effusion.BTP.Pieces
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
      torrent: Pieces.new(meta.info_hash),
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
  def blocks(d) do
    Pieces.unfinished(d.torrent)
  end

  @doc """
  Get the set of listeners waiting for this torrent to finish.
  """
  def listeners(d) do
    d.listeners
  end

  @doc """
  Get the torrent that this download is downloading.
  """
  def torrent(d) do
    d.torrent
  end

  @doc """
  Add a block of data to this download.

  This may trigger messages to be sent to any connections associated with this download's torrent.
  """
  def add_block(d, block, from) when is_peer_id(from) do
    {d, cancel_messages} = cancel_block_requests(d, block, from)

    torrent = Pieces.add_block(d.torrent, block)
    verified = Pieces.verified(torrent)

    have_messages = verified
    |> Enum.map(&({:broadcast, {:have, &1.index}}))

    write_messages = torrent
    |> Pieces.verified()
    |> Enum.map(fn p -> {:write_piece, torrent.info, d.file, p} end)

    {
      %{d | torrent: torrent},
      write_messages ++ have_messages ++ cancel_messages
    }
  end

  def mark_piece_written(d, i) do
    Map.update(d, :torrent, Pieces.new(d.meta.info_hash), &Pieces.mark_piece_written(&1, i))
  end

  defp cancel_block_requests(d, block, from) do
    block_id = Block.id(block)

    messages =
      d
      |> Map.get(:requested_pieces, MapSet.new())
      |> Map.get(block_id, MapSet.new())
      |> Enum.filter(&(&1 != from))
      |> Enum.map(&({:btp_send, &1, {:cancel, block_id}}))

    d = Map.update(d, :requested_pieces, Map.new(), &Map.delete(&1, block_id))

    {d, messages}
  end

  @doc """
  Add a process that should be notified when this download completes or crashes.
  """
  def add_listener(d, from) do
    Map.update(d, :listeners, MapSet.new(), &MapSet.put(&1, from))
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
  def done?(d) do
    Pieces.all_present?(d.torrent)
  end

  @doc """
  Get the next piece that this download should ask for.
  """
  def next_request(d) do
    next_block =
      Effusion.BTP.PieceSelection.next_block(
        d.torrent,
        Map.values(d.peers),
        # actual_connected_peers,
        @block_size
      )

    case next_block do
      nil -> {nil, d}
      _ ->
        s1 = Map.update(d, :requested_pieces, MapSet.new(), &MapSet.put(&1, next_block))
        {next_block, s1}
    end
  end

  defp next_request_msg(session) do
    case next_request(session) do
      {{peer_id, %{index: i, offset: o, size: sz}}, session} -> {session, [{:btp_send, peer_id, {:request, i, o, sz}}]}
      {nil, session} -> {session, []}
    end
  end

  @doc """
  Announce an event to this download's tracker,
  using the given Tracker HTTP Protocol (THP) client.
  """
  def announce(%{} = d, client, event \\ :interval) do
    {local_host, local_port} = Map.fetch!(d, :local_address)

    {:ok, res} =
      client.announce(
        d.meta.announce,
        local_host,
        local_port,
        d.peer_id,
        d.meta.info_hash,
        0,
        Pieces.bytes_completed(d.torrent),
        Pieces.bytes_left(d.torrent),
        event,
        d.tracker_id
      )

    known_addrs = Map.keys(d.peers)
    {known_ids, _} = Map.split(d.peer_addresses, known_addrs)

    new_peers =
      res.peers
      |> Enum.map(fn p ->
        Peer.new({p.ip, p.port}, d.peer_id, d.meta.info_hash, self())
        |> Peer.set_remote_peer_id(Map.get(p, :peer_id, nil))
      end)
      |> Enum.reject(&Enum.member?(known_ids, &1.remote_peer_id))
      |> Enum.reject(&Enum.member?(known_addrs, &1.address))
      |> Map.new(&({&1.address, &1}))

    announced_addrs = res.peers
    |> Enum.map(&({&1.ip, &1.port}))
    |> Enum.uniq()
    |> Enum.into([])

    all_peers = Map.merge(new_peers, d.peers)
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
        d.tracker_id
      end


    d = d
    |> Map.put(:peers, all_updated_peers)
    |> Map.put(:peer_addresses, peer_addresses)
    |> Map.put(:tracker_id, tracker_id)

    {d, res}
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session, thp_client) do
    {session, response} = announce(session, thp_client, :started)
    messages = Enum.map(session.peers, fn {_addr, p} -> {:btp_connect, p} end)
    {session, response, messages}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %{peer_id: peer_id}, remote_peer_id, msg)
      when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    case session_handle_message(d, remote_peer_id, msg) do
      {:error, reason} ->
        {:error, reason}

      {d, session_messages} ->
        {d, peer_messages} = delegate_message(d, remote_peer_id, msg)
        {d, session_messages ++ peer_messages}
    end
  end

  defp session_handle_message(d, _remote_peer_id, {:bitfield, b}) do
    pieces_count = Enum.count(d.meta.info.pieces)
    max_i = Enum.max(IntSet.new(b), fn -> 0 end)

    if max_i in 0..(pieces_count - 1) do
      {d, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(d, _remote_peer_id, {:have, i}) do
    pieces_count = Enum.count(d.meta.info.pieces)

    if i in 0..(pieces_count - 1) do
      {d, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(d, remote_peer_id, {:piece, b}) do
    {d, block_messages} = add_block(d, b, remote_peer_id)
    {d, request_messages} = next_request_msg(d)
    {d, block_messages ++ request_messages}
  end

  defp session_handle_message(d, _remote_peer_id, :unchoke) do
    next_request_msg(d)
  end

  defp session_handle_message(d, _remote_peer_id, _msg), do: {d, []}

  defp delegate_message(d, remote_peer_id, msg)
       when is_peer_id(remote_peer_id) do

    with {:ok, peer} <- get_connected_peer(d, remote_peer_id),
         {peer, responses} <- Peer.recv(peer, msg),
         d = Map.update(d, :peers, Map.new(), &Map.put(&1, peer.address, peer)) do
      {d, Enum.map(responses, fn r -> {:btp_send, remote_peer_id, r} end)}
    else
      _ -> {d, []}
    end
  end

  defp get_connected_peer(d, remote_peer_id)
       when is_peer_id(remote_peer_id) do
    with {:ok, address} <- Map.fetch(d.peer_addresses, remote_peer_id) do
      Map.fetch(d.peers, address)
    else
      _ -> {:error, :peer_not_found}
    end
  end

  defp peer(d, peer_id, peer_address)
       when is_peer_id(peer_id) do
    Peer.new(peer_address, d.peer_id, d.meta.info_hash, self())
    |> Map.put(:remote_peer_id, peer_id)
  end

  def handle_connect(d, peer_id, address)
      when is_peer_id(peer_id) do
    _ = Logger.debug("Handling connection success to #{inspect(address)}")
    d
    |> Map.update(:peers, Map.new(), &Map.put(&1, address, peer(d, peer_id, address)))
    |> Map.update(:peer_addresses, Map.new(), &Map.put(&1, peer_id, address))
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(d, peer_id, address)
      when is_peer_id(peer_id) do
    d
    |> Map.update(:peers, Map.new(), &Map.delete(&1, address))
    |> increment_connections()
  end

  defp increment_connections(d) do
    selected = Effusion.BTP.PeerSelection.select_peer(
      d.peer_id,
      d.peers,
      []
    )

    case selected do
      nil -> {d, []}
      peer -> {d, [{:btp_connect, peer}]}
    end
  end
end
