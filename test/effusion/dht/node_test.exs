defmodule Effusion.DHT.NodeTest do
  use ExUnit.Case
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.Node
  alias Effusion.Repo
  import Ecto.Query
  doctest Effusion.DHT.Node

  @bucket_max Bucket.max_bucket_upper_value()
  @bucket_middle trunc(Bucket.max_bucket_upper_value() / 2)

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
  end

  setup do
    Bucket.split(<<@bucket_middle::160>>)
    :ok
  end

  test "cannot insert a node into a bucket it doesn't fit in" do
    [_lower, upper] = Repo.all(from bucket in Bucket, order_by: fragment("lower(?)",bucket.range))

    expected_message =
      "ERROR P0001 (raise_exception) Node must be inserted into a bucket that contains its node ID. " <>
      "Node ID was 1 but target bucket range was " <>
      "[#{@bucket_middle},#{@bucket_max})"

    assert_raise Postgrex.Error, expected_message, fn ->
      %Node{
        node_id: <<1::160>>,
        bucket_id: upper.id,
        address: %Postgrex.INET{address: {127, 0, 0, 1}},
        port: 5000
      }
      # Just wrap it in a changeset since Node.changeset automatically inserts it into the correct bucket
      |> Ecto.Changeset.change([])
      |> Repo.insert!
    end
  end
end
