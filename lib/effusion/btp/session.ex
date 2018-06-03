defmodule Effusion.BTP.Session do
  alias Effusion.BTP.Torrent

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
    %{s | torrent: torrent}
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

  def increment_connections(s) do
    case select_peer(s) do
      nil -> s
      peer ->
        {:ok, _socket} = connect_to_peer(s, peer)
        Map.update!(s, :connected_peers, &Map.put(&1, peer.info_hash, peer))
    end
  end

  defp select_peer(s) do
    s.peers
       |> Enum.find(fn(p) -> Map.get(p, :peer_id) != s.peer_id end)
  end

  defp connect_to_peer(s, peer) do
    Effusion.Application.PeerServer.connect(
      {peer.ip, peer.port},
      s.peer_id,
      s.meta.info_hash,
      self()
    )
  end
end
