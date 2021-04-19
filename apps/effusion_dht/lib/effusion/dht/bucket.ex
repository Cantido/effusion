defmodule Effusion.DHT.Bucket do
  alias Effusion.DHT.Node

  defstruct [
    range: 0..1461501637330902918203684832716283019655932542976,
    nodes: []
  ]

  def new do
    %__MODULE__{}
  end

  def node_in_range?(%__MODULE__{range: range}, %Node{id: id}) do
    id_int = :crypto.bytes_to_integer(id)

    id_int in range
  end

  def add_node(bucket, node) do
    if Enum.count(bucket.nodes) < 8 do
      %__MODULE__{ bucket |
        nodes: [node | bucket.nodes]
      }
    else
      bucket
    end
  end
end
