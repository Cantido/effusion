defmodule Effusion.DHT.Server do
  use Effusion.DHT.KRPC.Server

  alias Effusion.DHT.KRPC

  defstruct [
    :node_id
  ]

  def new(node_id) do
    %__MODULE__{node_id: node_id}
  end

  def handle_krpc_message(message, state) do
    msg = KRPC.decode!(message)
    context = %{}

    state = DHT.handle_message(state, message, context)

    {:noreply, state}
  end
end
