defmodule Effusion.CQRS.Aggregates.Node do
  alias Effusion.CQRS.Commands.StartDHTNode
  alias Effusion.CQRS.Events.DHTNodeStarted

  defstruct [
    node_id: nil,
    routing_table: nil
  ]

  def execute(
    %__MODULE__{node_id: nil},
    %StartDHTNode{node_id: node_id}
  ) do
    %DHTNodeStarted{node_id: node_id}
  end

  def execute(%__MODULE__{}, %StartDHTNode{}) do
    {:error, :node_already_exists}
  end

  def apply(
    %__MODULE__{node_id: nil} = node,
    %DHTNodeStarted{node_id: node_id}
  ) do
    %__MODULE__{node |
      node_id: node_id
    }
  end
end
