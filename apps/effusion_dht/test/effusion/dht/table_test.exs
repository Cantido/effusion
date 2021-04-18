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

    node = %Node{
      id: id,
      host: {127, 0, 0, 1},
      port: Enum.random(1024..65535)
    }

    table =
      Table.new(local_id)
      |> Table.add(node)

    [result_node] = Table.take_closest_to(table, id, 1)

    assert result_node.id == id
  end
end
