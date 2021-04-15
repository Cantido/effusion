defmodule Effusion.DHT do
  @moduledoc """
  Documentation for Effusion.DHT.
  """

  alias Effusion.DHT.Messages.Ping
  alias Effusion.DHT.Node

  @enforce_keys [:local_id]
  defstruct [
    local_id: nil,
    nodes: Map.new()
  ]

  defguard is_node_id(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_info_hash(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_inet_port(n) when is_integer(n) and n in 1..65_535

  def new do
    %__MODULE__{
      local_id: Base.decode64!(Application.fetch_env!(:effusion, :dht_node_id))
    }
  end

  def handle_message(dht, %Ping.Query{sender_id: sender_id}, _context) do
    nodes =
      Map.update(
        dht.nodes,
        sender_id,
        %Node{id: sender_id, last_query_received_at: DateTime.utc_now()},
        fn node ->
          %Node{node | last_query_received_at: DateTime.utc_now()}
        end
      )

    dht = %__MODULE__{dht | nodes: nodes}

    {:reply, %Ping.Response{sender_id: dht.local_id}, dht}
  end
end
