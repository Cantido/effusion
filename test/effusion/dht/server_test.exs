defmodule Effusion.DHT.ServerTest do
  use ExUnit.Case
  alias Effusion.DHT.Server
  alias Effusion.Repo

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  @node_id Application.get_env(:effusion, :dht_node_id) |> Base.decode64!()

  test "handle find_node query" do
    {:ok, node} = Repo.insert(%Effusion.DHT.Node{
      node_id: "09876543210987654321",
      address: %Postgrex.INET{address: {192, 168, 1, 1}},
      port: 65535,
      last_contacted: DateTime.truncate(Timex.now(), :second)
    })
    node = Effusion.DHT.Node.compact(node)

    query = {:find_node, "abcde", "12345678901234567890", "09876543210987654321"}
    {:find_node, transaction_id, node_id, nodes} = Server.handle_krpc_query(query)

    assert transaction_id == "abcde"
    assert node_id == @node_id
    assert nodes == [node]
  end
end
