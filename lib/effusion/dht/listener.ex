defmodule Effusion.DHT.Listener do
  use GenServer
  alias Effusion.CQRS.Contexts.DHT, as: DHTContext
  alias Effusion.DHT.KRPC.{Query, Response}

  def start_link(opts) do
    port = Keyword.fetch!(opts, :port)
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  def init(port) do
    {:ok, _socket} = :gen_udp.open(port, active: :once)
    {:ok, []}
  end

  def handle_info({:udp, _socket, _ip, _port, packet}, state) do
    {:ok, de_bencoded} = Bento.decode(packet)

    case Map.get(de_bencoded, "y") do
      "q" ->
        Task.start(
          Effusion.DHT.Handler,
          :handle_query,
          [Query.decode(de_bencoded)])
      "r" ->
        Task.start(
          Effusion.DHT.Handler,
          :handle_response,
          [Response.decode(de_bencoded)])
    end
    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end
end
