defmodule Effusion.DHT.Bucket do
  alias Effusion.DHT.Node
  @max_node_count 8

  defstruct [
    range: 0..1461501637330902918203684832716283019655932542976,
    nodes: %{}
  ]

  def new do
    %__MODULE__{}
  end

  def new(range) do
    %__MODULE__{range: range}
  end

  def size(%__MODULE__{nodes: nodes}) do
    Enum.count(nodes)
  end

  def nodes(%__MODULE__{nodes: nodes}) do
    Map.values(nodes)
  end

  def member?(%__MODULE__{nodes: nodes}, id) do
    Map.has_key?(nodes, id)
  end

  def full?(bucket) do
    size(bucket) >= @max_node_count
  end

  def id_in_range?(%__MODULE__{range: range}, id) when is_binary(id) do
    id_int = :crypto.bytes_to_integer(id)

    id_int in range
  end

  def add_node(bucket, node) do
    unless id_in_range?(bucket, node.id) do
      raise "Node with ID #{Base.encode16(node.id)} does not in bucket's range of #{inspect bucket.range}"
    end

    cond do
      Map.has_key?(bucket.nodes, node.id) ->
        %__MODULE__{ bucket |
          nodes: Map.put(bucket.nodes, node.id, node)
        }

      full?(bucket) ->
        bad_node = Enum.find(Map.values(bucket.nodes), &Node.bad?/1)
        if bad_node do
          nodes =
            Map.delete(bucket.nodes, bad_node.id)
            |> Map.put(node.id, node)

          %__MODULE__{ bucket |
            nodes: nodes
          }
        else
          bucket
        end
      true ->
        %__MODULE__{ bucket |
          nodes: Map.put(bucket.nodes, node.id, node)
        }
    end
  end

  def split(bucket) do
    midpoint = trunc(bucket.range.last / 2)

    first = new(bucket.range.first..midpoint)
    last = new(midpoint..bucket.range.last)

    {low_nodes, high_nodes} = Enum.split_with(nodes(bucket), &id_in_range?(first, &1.id))

    first = Enum.reduce(low_nodes, first, &add_node(&2, &1))
    last = Enum.reduce(high_nodes, last, &add_node(&2, &1))

    [first, last]
  end
end
