defmodule Effusion.DHT.Table do
  alias Effusion.DHT.Node

  defstruct [
    nodes: []
  ]

  def new do
    %__MODULE__{}
  end

  def new(nodes) when is_list(nodes) do
    %__MODULE__{
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
