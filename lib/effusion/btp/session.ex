defmodule Effusion.BTP.Session do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Peer

  @local_peer_id "Effusion Experiment!"

  @block_size Application.get_env(:effusion, :block_size)

  def new(meta, local_server, file \\ nil) do
    %{
      file: file,
      meta: meta,
      torrent: Torrent.new(meta.info),
      local_peer: local_server,
      peers: MapSet.new(),
      connected_peers: Map.new(),
      peer_id: @local_peer_id,
      listeners: MapSet.new(),
      requested: MapSet.new()
    }
  end

  def handle_msg(s, peer_id, msg) do
    peer = Map.get(s.connected_peers, peer_id)
    case peer do
      nil -> {s, []}
      p ->
        {peer, messages} = Peer.recv(p, msg)
        {%{s | connected_peers: Map.put(s.connected_peers, peer_id, peer)}, messages}
    end
  end

  def blocks(s) do
    Torrent.blocks(s.torrent)
  end

  def listeners(s) do
    s.listeners
  end

  def torrent(s) do
    s.torrent
  end

  def add_block(s, block) do
    torrent = Torrent.add_block(s.torrent, block)
    s = %{s | torrent: torrent}
    write(s)
  end

  def add_listener(s, pid) do
    Map.update!(s, :listeners, &MapSet.put(&1, pid))
  end

  def write(s) do
    {:ok, torrent} = Torrent.write_to(s.torrent, s.file)
    %{s | torrent: torrent}
  end

  def done?(s) do
    Torrent.done?(s.torrent)
  end

  def next_request(s) do
    have_pieces = Torrent.finished_pieces(s.torrent)
    next_block = Effusion.BTP.PieceSelection.next_block(s.meta.info, have_pieces, @block_size)
    s1 = Map.update!(s, :requested, &MapSet.put(&1, next_block))

    {next_block, s1}
  end

  def announce(s, client) do
    {local_host, local_port} = s.local_peer

    {:ok, res} = client.announce(
      s.meta.announce,
      local_host,
      local_port,
      s.peer_id,
      s.meta.info_hash,
      0,
      0,
      s.meta.info.length
    )

    %{s | peers: res.peers}
  end

  def start(session, thp_client) do
    session
    |> announce(thp_client)
    |> increment_connections()
  end

  defp next_request_msg(s) do
    case next_request(s) do
      %{index: i, offset: o, size: s} -> [{:request, i, o, s}]
      :done -> []
    end
  end

  def recv(s, peer_id, {:piece, b} = msg) do
    s = add_block(s, b)
    session_messages = next_request_msg(s)
    {s, peer_messages} = delegate_recv(s, peer_id, msg)
    {s, session_messages ++ peer_messages}
  end

  def recv(s, peer_id, :unchoke = msg) do
    session_messages = next_request_msg(s)
    {s, peer_messages} = delegate_recv(s, peer_id, msg)
    {s, session_messages ++ peer_messages}
  end

  def recv(s, peer_id, msg) do
    delegate_recv(s, peer_id, msg)
  end

  defp delegate_recv(s, peer_id, msg) do
    peer = Map.fetch!(s.connected_peers, peer_id)
    {peer, responses} = Peer.recv(peer, msg)

    session = add_connected_peer(s, peer)
    {session, Map.new([{peer.peer_id, responses}])}
  end

  def add_connected_peer(s, peer) do
    Map.update!(s, :connected_peers, &Map.put(&1, peer.peer_id, peer))
  end

  def increment_connections(s) do
    case select_peer(s) do
      nil -> s
      peer ->
        {:ok, _socket} = connect_to_peer(s, peer)
        add_connected_peer(s, peer)
    end
  end

  defp select_peer(s) do
    s.peers
       |> Enum.find(fn(p) -> Map.get(p, :peer_id) != s.peer_id end)
  end

  defp connect_to_peer(s, peer) do
    Effusion.Application.Connection.connect(
      Peer.new(
        {peer.ip, peer.port},
        s.peer_id,
        s.meta.info_hash,
        self())
      )
  end
end
