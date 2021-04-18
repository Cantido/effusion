defmodule Effusion.DHT.Table do
  alias Effusion.DHT.Node

  @enforce_keys [
    :local_id
  ]
  defstruct [
    local_id: nil,
    nodes: []
  ]

  def new(local_id) do
    %__MODULE__{
      local_id: local_id
    }
  end

  def new(local_id, nodes) when is_list(nodes) do
    %__MODULE__{
      local_id: local_id,
      nodes: nodes
    }
  end

  def add(table = %__MODULE__{nodes: nodes}, node) do
    %__MODULE__{ table | nodes: [node | nodes]}
  end

  def take_closest_to(%__MODULE__{nodes: nodes}, target, count) do
    nodes
    |> Enum.sort_by(&Node.distance(target, &1.id))
    |> Enum.take(count)
  end
end
