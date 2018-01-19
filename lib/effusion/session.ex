defmodule Effusion.Session do
  alias Effusion.Metainfo
  alias Effusion.LocalPeer
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Messages.Handshake

  def start(meta_bin) when is_binary(meta_bin) do
    {:ok, meta} = Metainfo.decode(meta_bin)

    tracker_request = %{
      info_hash: meta.info_hash,
      peer_id: LocalPeer.peer_id(),
      port: 4040,
      uploaded: 0,
      downloaded: 0,
      left: 1899528192,
      ip: to_string(:inet.ntoa(LocalPeer.ip_address()))
    }

    http_res = HTTPotion.get("http://localhost:6969?" <> URI.encode_query(tracker_request))
    {:ok, res} = ExBencode.decode(http_res.body)

    peer = select_peer(res["peers"], LocalPeer.peer_id())

    connect(peer, meta)
  end

  def connect(peer, meta) do
    opts = [packet: 0, active: false]

    {:ok, peer_ip} = :inet.parse_address to_charlist(peer["ip"])
    peer_port = peer["port"]

    IO.puts "trying to connect to IP #{inspect(peer_ip)}"
    IO.puts "port: #{inspect(peer_port)}"

    {:ok, socket} = :gen_tcp.connect(peer_ip, peer_port, opts)
    :ok = :gen_tcp.send(socket, Handshake.encode(LocalPeer.peer_id(), meta.info_hash))
    {:ok, handshake} = :gen_tcp.recv(socket, 68, 5000)

    IO.puts "I got a handshake!! #{inspect(handshake)}"

    :inet.setopts(socket, [packet: 4])

    {:ok, data} = :gen_tcp.recv(socket, 0)

    IO.puts "Received package: #{inspect(data)}"

    {:ok, message} = Messages.decode(IO.iodata_to_binary(data))

    IO.puts "I got a message: #{inspect(message)}"
  end

  def select_peer(peers, peer_id) do
    IO.puts "Peers to choose from: #{inspect(peers, pretty: true)}"
    peers |> Enum.find(fn(p) -> p["peer_id"] != peer_id end)
  end
end
