defmodule Effusion.BTP.Session do
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Peer
  require Logger

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
    s = %{s | torrent: torrent}
    write(s)
  end

  def add_listener(s, pid) when is_pid(pid) do
    Map.update!(s, :listeners, &MapSet.put(&1, pid))
  end

  def each_listener(%{listeners: listeners}, fun) do
    Enum.each(listeners, &fun.(&1))
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

    peers =
      Enum.map(
        res.peers,
        fn p ->
          Peer.new(
            {p.ip, p.port},
            p.peer_id,
            s.meta.info_hash,
            self())
        end)

    %{s | peers: peers}
  end

  def start(session, thp_client) do
    session
    |> announce(thp_client)
    |> increment_connections()
  end

  defp next_request_msg(session) do
    case next_request(session) do
      {%{index: i, offset: o, size: sz}, session} -> {[{:request, i, o, sz}], session}
      {:done, session} -> {[], session}
    end
  end

  def handle_message(s = %{peer_id: peer_id}, remote_peer_id, {:piece, b} = msg) when peer_id != remote_peer_id do
    s = add_block(s, b)
    {session_messages, s} = next_request_msg(s)
    {s, peer_messages} = delegate_message(s, remote_peer_id, msg)
    {s, session_messages ++ peer_messages}
  end

  def handle_message(s = %{peer_id: peer_id}, remote_peer_id, :unchoke = msg) when peer_id != remote_peer_id do
    {session_messages, s} = next_request_msg(s)
    {s, peer_messages} = delegate_message(s, remote_peer_id, msg)
    messages = session_messages ++ peer_messages
    Logger.debug("Got unchoke, so sending these request messages: #{inspect(messages)}")
    {s, session_messages ++ peer_messages}
  end

  def handle_message(s = %{peer_id: peer_id}, remote_peer_id, msg) when peer_id != remote_peer_id do
    delegate_message(s, remote_peer_id, msg)
  end

  defp delegate_message(s = %{peer_id: peer_id}, remote_peer_id, msg) when peer_id != remote_peer_id do
    peer = get_connected_peer(s, remote_peer_id)
    {peer, responses} = Peer.recv(peer, msg)

    session = add_connected_peer(s, peer)
    {session, responses}
  end

  defp get_connected_peer(s = %{peer_id: peer_id}, remote_peer_id) when peer_id != remote_peer_id do
    s
    |> Map.get(:connected_peers, Map.new())
    |> Map.get(remote_peer_id, default_peer(s, remote_peer_id))
  end

  defp default_peer(%{peer_id: peer_id, meta: %{info_hash: info_hash}}, remote_peer_id) when peer_id != remote_peer_id do
    Peer.new(
      {nil, nil},
      peer_id,
      info_hash,
      self())
    |> Peer.set_remote_peer_id(remote_peer_id)
  end

  def add_connected_peer(s, peer_id, peer_address) do
    peer = Peer.new(
      peer_address,
      s.peer_id,
      s.meta.info_hash,
      self())
    |> Map.put(:remote_peer_id, peer_id)

    add_connected_peer(s, peer)
  end

  def add_connected_peer(s = %{peer_id: peer_id}, peer = %{remote_peer_id: remote_peer_id}) when peer_id != remote_peer_id do
    Map.update!(s, :connected_peers, &Map.put(&1, remote_peer_id, peer))
  end

  def remove_connected_peer(s, peer_id) do
    Map.update!(s, :connected_peers, &Map.delete(&1, peer_id))
  end

  defp increment_connections(s) do
    case select_peer(s) do
      nil -> s
      peer -> connect_to_peer(s, peer)
    end
  end

  defp select_peer(s) do
    s.peers
       |> Enum.reject(fn(p) -> Map.get(p, :remote_peer_id) == s.peer_id end)
       |> Enum.random()
  end

  defp connect_to_peer(s, peer) do
    {:ok, _} = Effusion.PWP.Connection.connect(peer)
    s
  end
end
