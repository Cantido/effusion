defmodule Effusion.DHT.BucketTest do
  use ExUnit.Case, async: true
  alias Effusion.DHT.Bucket
  doctest Effusion.DHT.Bucket

  test "A new bucket covers the entire ID range" do
    bucket = Bucket.new()

    assert bucket.range.first == 0
    assert bucket.range.last == 1461501637330902918203684832716283019655932542976
  end

  describe "add_node/2" do
    test "stores nodes" do
      node = TestHelper.generate_node()

      bucket =
        Bucket.new()
        |> Bucket.add_node(node)

      assert Enum.any?(bucket.nodes, & &1.id == node.id)
    end

    test "can't store more than eight" do
      ignored_node = TestHelper.generate_node()

      bucket =
        Bucket.new()
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(ignored_node)

      refute Enum.any?(bucket.nodes, & &1.id == ignored_node.id)
    end
  end
end
