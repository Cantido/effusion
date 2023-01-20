defmodule Effusion.DHT.TableTest do
  use ExUnit.Case, async: true
  alias Effusion.DHT.Table
  alias Effusion.DHT.Node
  doctest Effusion.DHT.Table

  setup do
    local_id = Node.generate_node_id()

    %{local_id: local_id}
  end

  test "stores nodes", %{local_id: local_id} do
    id = Node.generate_node_id()

    node = generate_node(id)

    table =
      Table.new(local_id)
      |> Table.add(node)

    [result_node] = Table.take_closest_to(table, id, 1)

    assert result_node.id == id
  end

  test "stores multiple buckets" do
    local_node_id = <<0::160>>

    table =
      Table.new(local_node_id)
      |> Table.add(generate_node(<<1::160>>))
      |> Table.add(generate_node(<<2::160>>))
      |> Table.add(generate_node(<<3::160>>))
      |> Table.add(generate_node(<<4::160>>))
      |> Table.add(generate_node(<<5::160>>))
      |> Table.add(generate_node(<<6::160>>))
      |> Table.add(generate_node(<<7::160>>))
      |> Table.add(generate_node(<<8::160>>))

    # Bucket should split after we add this one

    table = Table.add(table, generate_node(<<0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>))

    nodes = Table.nodes(table)

    assert Enum.any?(nodes, & &1.id == <<0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>)
  end

  test "new nodes are rejected if the bucket is full" do
    local_node_id = <<0::160>>

    table =
      Table.new(local_node_id)
      |> Table.add(generate_node(<<1::160>>))
      |> Table.add(generate_node(<<2::160>>))
      |> Table.add(generate_node(<<3::160>>))
      |> Table.add(generate_node(<<4::160>>))
      |> Table.add(generate_node(<<5::160>>))
      |> Table.add(generate_node(<<6::160>>))
      |> Table.add(generate_node(<<7::160>>))
      |> Table.add(generate_node(<<8::160>>))

    # Bucket should split after we add this one

    table = Table.add(table, generate_node(<<0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF>>))

    # Now we shouldn't be able to insert any more good nodes into our end of the table

    table = Table.add(table, generate_node(<<9::160>>))

    nodes = Table.nodes(table)

    refute Enum.any?(nodes, & &1.id == <<9::160>>)
  end

  defp generate_node(id) do
    %Node{
      id: id,
      host: {127, 0, 0, 1},
      port: Enum.random(1024..65535)
    }
    |> Node.response_received_at(~U[2021-04-18T14:11:00Z])
  end
end
