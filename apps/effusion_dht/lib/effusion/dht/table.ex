defmodule Effusion.DHT.Table do
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.Node

  @enforce_keys [
    :local_id
  ]
  defstruct [
    local_id: nil,
    buckets: [Bucket.new()]
  ]

  def new(local_id) do
    %__MODULE__{
      local_id: local_id
    }
  end

  def new(local_id, nodes) when is_list(nodes) do
    nodes
    |> Enum.reduce(new(local_id), fn node, table ->
      add(table, node)
    end)
  end

  def add(table = %__MODULE__{buckets: buckets}, node) do
    bucket = Enum.find(buckets, &Bucket.id_in_range?(&1, node.id))

    if is_nil(bucket) do
      raise "Got a node with ID #{Base.encode16(node.id)} and it doesn't fit in any buckets!"
    end

    split_buckets =
      if Bucket.full?(bucket) and Bucket.id_in_range?(bucket, table.local_id) do
        bucket_index = Enum.find_index(buckets, &Bucket.id_in_range?(&1, node.id))
        buckets =
          List.update_at(buckets, bucket_index, &Bucket.split/1)
          |> List.flatten()
      else
        buckets
      end

    bucket_index = Enum.find_index(split_buckets, &Bucket.id_in_range?(&1, node.id))

    if is_nil(bucket_index) do
      raise "Got a node with ID #{Base.encode16(node.id)} and it doesn't fit in any buckets after we split them!"
    end

    updated_buckets = List.update_at(split_buckets, bucket_index, &Bucket.add_node(&1, node))

    %__MODULE__{ table | buckets: updated_buckets}
  end

  def take_closest_to(%__MODULE__{buckets: buckets}, target, count) do
    buckets
    |> Enum.flat_map(& &1.nodes)
    |> Enum.sort_by(&Node.distance(target, &1.id))
    |> Enum.take(count)
  end

  def nodes(%__MODULE__{buckets: buckets}) do
    Enum.flat_map(buckets, & &1.nodes)
  end
end
