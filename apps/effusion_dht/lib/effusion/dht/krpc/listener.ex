defmodule Effusion.DHT.KRPC.Server do
  use GenServer
  alias Effusion.DHT.Server

  @callback handle_krpc_message(message :: term, state ::term) :: term

  def start_link(opts) do
    port = Keyword.fetch!(opts, :port)
    GenServer.start_link(__MODULE__, port, name: __MODULE__)
  end

  def handle_info({:udp, socket, ip, port, packet}, state) do
    message = Effusion.DHT.KRPC.Message.decode!(packet)
    state = handle_krpc_message(message, state)

    {:noreply, state}
  end

  def handle_info({:udp_passive, socket}, state) do
    :ok = :inet.setopts(socket, active: :once)
    {:noreply, state}
  end
end
