defmodule Effusion.DHT.NodeTest do
  use ExUnit.Case
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.Node
  alias Effusion.Repo
  import Ecto.Query
  doctest Effusion.DHT.Node

  @bucket_max 1461501637330902918203684832716283019655932542976
  @bucket_middle 730750818665451459101842416358141509827966271488

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
  end

  setup do
    Repo.insert_all(Bucket, [
      %{range: [0, @bucket_middle]},
      %{range: [@bucket_middle, @bucket_max+1]
    }])
    :ok
  end

  test "cannot insert a node into a bucket it doesn't fit in" do
    [_lower, upper] = Repo.all(from bucket in Bucket, order_by: fragment("lower(?)",bucket.range))

    expected_message =
      "ERROR P0001 (raise_exception) Node must be inserted into a bucket that contains its node ID. " <>
      "Node ID was 1 but target bucket range was " <>
      "[730750818665451459101842416358141509827966271488,1461501637330902918203684832716283019655932542977)"

    assert_raise Postgrex.Error, expected_message, fn ->
      %Node{}
      |> Node.changeset(%{
        node_id: 1,
        bucket_id: upper.id,
        address: {127, 0, 0, 1},
        port: 5000
      })
      |> Repo.insert!
    end
  end
end
