defmodule Effusion.Session do
  use GenServer

  alias Effusion.Metainfo
  alias Effusion.LocalPeer
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Messages.Handshake

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  def start_link([metabin, peer_id, ip, port]) when is_binary(metabin) do
    GenServer.start_link(__MODULE__, [metabin, peer_id, ip, port])
  end

  def announce(pid) do
    GenServer.call(pid, :announce)
  end

  def select_peer(pid) do
    GenServer.call(pid, :select_peer)
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

    :ok = :inet.setopts(socket, [packet: 4])
    {:ok, data} = :gen_tcp.recv(socket, 0)
    {:ok, message} = Messages.decode(IO.iodata_to_binary(data))
  end

  def select_peer(peers, peer_id) do
    IO.puts "Peers to choose from: #{inspect(peers, pretty: true)}"
    peers |> Enum.find(fn(p) -> p["peer_id"] != peer_id end)
  end

  ## Callbacks

  def init([meta_bin, peer_id, ip, port]) do
    {:ok, meta} = Metainfo.decode(meta_bin)

    state = %{
      meta: meta,
      ip: ip,
      port: port,
      peer_id: peer_id
    }

    {:ok, state}
  end

  def handle_call(:announce, _from, state) do
    {:ok, res} = @thp_client.announce(
      state.meta.announce,
      state.ip,
      state.port,
      state.peer_id,
      state.meta.info_hash,
      0,
      0,
      state.meta.info.length
    )

    state1 = Map.put(state, :peers, res.peers)

    {:reply, :ok, state1}
  end

  def handle_call(:select_peer, _from, state) do
    peer = state.peers
        |> Enum.find(fn(p) -> p["peer_id"] != state.peer_id end)

    {:reply, peer, state}
  end
end
