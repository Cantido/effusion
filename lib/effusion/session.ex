defmodule Effusion.Session do
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
      peer_id: @local_peer_id,
      listeners: MapSet.new()
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
    MapSet.put(s.listeners, pid)
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

    next_block
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

  def select_peer(s) do
    s.peers
       |> Enum.find(fn(p) -> Map.get(p, :peer_id) != s.peer_id end)
  end

  def connect_to_peer(s, peer) do
    Effusion.PWP.Peer.connect(
      {peer.ip, peer.port},
      s.peer_id,
      s.meta.info_hash,
      self()
    )
  end
end
