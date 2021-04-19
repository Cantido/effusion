defmodule Effusion.DHT.Bucket do
  alias Effusion.DHT.Node
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
    unless id_in_range?(bucket, node.id) do
      raise "Node with ID #{Base.encode16(node.id)} does not in bucket's range of #{inspect bucket.range}"
    end

    if full?(bucket) do
      first_bad_index = Enum.find_index(bucket.nodes, &Node.bad?/1)
      if first_bad_index do
        nodes = List.replace_at(bucket.nodes, first_bad_index, node)

        %__MODULE__{ bucket |
          nodes: nodes
        }
      else
        bucket
      end
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
