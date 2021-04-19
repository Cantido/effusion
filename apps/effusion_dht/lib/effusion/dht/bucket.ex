defmodule Effusion.DHT.Bucket do
  @max_node_count 8

  defstruct [
    range: 0..1461501637330902918203684832716283019655932542976,
    nodes: []
  ]

  def new do
    %__MODULE__{}
  end

  def new(range) do
    %__MODULE__{range: range}
  end

  def full?(%__MODULE__{nodes: nodes}) do
    Enum.count(nodes) >= @max_node_count
  end

  def id_in_range?(%__MODULE__{range: range}, id) when is_binary(id) do
    id_int = :crypto.bytes_to_integer(id)

    id_int in range
  end

  def add_node(bucket, node) do
    if full?(bucket) do
      bucket
    else
      %__MODULE__{ bucket |
        nodes: [node | bucket.nodes]
      }
    end
  end

  def split(bucket) do
    midpoint = trunc(bucket.range.last / 2)

    first = new(bucket.range.first..midpoint)
    last = new(midpoint..bucket.range.last)

    {low_nodes, high_nodes} = Enum.split_with(bucket.nodes, &id_in_range?(first, &1.id))

    first = Enum.reduce(low_nodes, first, &add_node(&2, &1))
    last = Enum.reduce(high_nodes, last, &add_node(&2, &1))

    [first, last]
  end
end
