defmodule Effusion.DHT.ServerManager do
  use GenServer
  alias Effusion.DHT.Server

  def start_link(opts) do
    node_id = Keyword.fetch!(opts, :node_id)
    GenServer.start_link(__MODULE__, node_id, opts)
  end

  def init(node_id) do
    {:ok, Server.new(node_id)}
  end


  def handle_message(server \\ __MODULE__, message, context) do
    GenServer.call(server, {:handle_message, message, context})
  end

  def handle_call({:handle_message, message, context}, _from, state) do
    {reply, next_state} = Server.handle_message(state, message, context)
    {:reply, reply, next_state}
  end
end
