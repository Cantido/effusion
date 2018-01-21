defmodule Effusion.Session do
  use GenServer

  alias Effusion.Metainfo
  alias Effusion.LocalPeer
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Messages.Handshake

  @thp_client Application.get_env(:effusion, :thp_client)

  def start_linuxmint do
    {:ok, meta} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    start(meta)
  end

  def init(meta_bin, peer_id, ip, port) do
    {:ok, meta} = Metainfo.decode(meta_bin)
    {
      :ok,
      %{
        meta: meta,
        ip: ip,
        port: port,
        peer_id: peer_id
      }
    }
  end

  def start(meta_bin) when is_binary(meta_bin) do
    peer_id = "Effusion Experiment!"

    {:ok, meta} = Metainfo.decode(meta_bin)

    {:ok, res} = @thp_client.announce(
      meta.announce,
      {127, 0, 0, 1},
      4040,
      peer_id,
      meta.info_hash,
      0,
      0,
      1899528192
    )

    :ok

    # peer = select_peer(res["peers"], peer_id)

    # connect(peer, meta)
  end

  def connect(peer, meta) do
    {:ok, peer_ip} = :inet.parse_address to_charlist(peer["ip"])
    peer_port = peer["port"]

    ## Handshake

    {:ok, socket} = :gen_tcp.connect(peer_ip, peer_port, [packet: 0, active: false])
    :ok = :gen_tcp.send(socket, Handshake.encode("Effusion Experiment!", meta.info_hash))
    {:ok, handshake} = :gen_tcp.recv(socket, 68, 5000)
    {:ok, hs} = Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake))

    ## Start processing messages

    :inet.setopts(socket, [packet: 4])
    {:ok, data} = :gen_tcp.recv(socket, 0)
    {:ok, message} = Messages.decode(IO.iodata_to_binary(data))
  end

  def select_peer(peers, peer_id) do
    IO.puts "Peers to choose from: #{inspect(peers, pretty: true)}"
    peers |> Enum.find(fn(p) -> p["peer_id"] != peer_id end)
  end
end
