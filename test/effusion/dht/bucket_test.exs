defmodule Effusion.DHT.BucketTest do
  use ExUnit.Case
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.Node
  alias Effusion.Repo
  import Ecto.Query
  doctest Effusion.DHT.Bucket

  @bucket_max 1461501637330902918203684832716283019655932542976
  @bucket_middle 730750818665451459101842416358141509827966271488

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
  end

  test "insert the first bucket" do
    %Bucket{}
    |> Bucket.changeset(%{
      range: [0, @bucket_max+1]
    })
    |> Repo.insert!()
  end

  test "buckets can be split" do
    %Bucket{}
    |> Bucket.changeset(%{
      range: [0, @bucket_max+1]
    })
    |> Repo.insert!()

    [lower, upper] = Bucket.split(<<@bucket_middle::160>>)

    assert lower.range.lower == Decimal.new(0)
    assert lower.range.upper == Decimal.new(@bucket_middle)
    assert upper.range.lower == Decimal.new(@bucket_middle)
    assert upper.range.upper == Decimal.new(@bucket_max+1)
  end

  test "splitting a bucket maintains node references" do
    bucket = %Bucket{}
    |> Bucket.changeset(%{
      range: [0, @bucket_max+1]
    })
    |> Repo.insert!()

    %Node{}
    |> Node.changeset(%{
        node_id: <<1::160>>,
        bucket_id: bucket.id,
        address: {127, 0, 0, 1},
        port: 5000
      })
    |> Repo.insert!

    %Node{}
    |> Node.changeset(%{
        node_id: <<(@bucket_max - 1)::160>>,
        bucket_id: bucket.id,
        address: {127, 0, 0, 2},
        port: 5000
      })
    |> Repo.insert!

    Ecto.Adapters.SQL.query!(Effusion.Repo, "SELECT * FROM split_bucket($1);", [@bucket_middle])

    [lower_bucket, upper_bucket] = Repo.all(from bucket in Bucket, order_by: fragment("lower(?)",bucket.range))
    # If the next line is returning an empty list,
    # then splitting the buckets lost the nodes
    [lower_node, upper_node] = Repo.all(from node in Node, order_by: node.node_id)

    assert lower_node.bucket_id == lower_bucket.id
    assert upper_node.bucket_id == upper_bucket.id
  end

  test "can insert multiple, but still spanning, buckets in a transaction" do
    Repo.insert_all(Bucket, [
      %{range: [0, @bucket_middle]},
      %{range: [@bucket_middle, @bucket_max+1]
    }])
  end

  test "insert a bucket that doesn't span the range" do
    Repo.delete_all(Bucket)

    expected_message = "ERROR P0001 (raise_exception) Bucket ranges must completely cover the range from 0 to 2^160 (inclusive). Coverage after inserting was [0,1)"

    assert_raise Postgrex.Error, expected_message, fn ->
      %Bucket{}
      |> Bucket.changeset(%{
        range: [0, 1]
      })
      |> Repo.insert!()
    end
  end
end
