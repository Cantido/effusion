defmodule Effusion.DHT.BucketTest do
  use ExUnit.Case, async: true
  alias Effusion.DHT.Bucket
  alias Effusion.DHT.Node
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

      assert Bucket.member?(bucket, node.id)
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

      refute Bucket.member?(bucket, ignored_node.id)
    end

    test "storing a new node after eight replaces bad nodes" do

      bad_node = generate_bad_node()
      replacing_node = TestHelper.generate_node()

      bucket =
        Bucket.new()
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(TestHelper.generate_node())
        |> Bucket.add_node(bad_node)
        |> Bucket.add_node(replacing_node)

      assert Bucket.member?(bucket, replacing_node.id)
      refute Bucket.member?(bucket, bad_node.id)
    end

    def generate_bad_node do
      Node.generate_node_id()
      |> Node.new({127, 0, 0, 1}, Enum.random(1024..65535))
      |> Node.failed_query()
      |> Node.failed_query()
      |> Node.failed_query()
      |> Node.failed_query()
      |> Node.failed_query()
    end

    test "throws an error if the node's ID is outside of the bucket's range" do
      bucket = Bucket.new(0..128)
      node = Node.new(<<255>>, {127, 0, 0, 1}, 8420)

      assert_raise RuntimeError, fn ->
        Bucket.add_node(bucket, node)
      end
    end

    test "replaces nodes with the same ID" do
      first = TestHelper.generate_node()
      second = TestHelper.generate_node(first.id)

      size =
        Bucket.new()
        |> Bucket.add_node(first)
        |> Bucket.add_node(second)
        |> Bucket.size()

      assert size == 1
    end
  end
end
