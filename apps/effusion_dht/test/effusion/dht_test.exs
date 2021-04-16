defmodule Effusion.DHTTest do
  use ExUnit.Case
  alias Effusion.DHT
  alias Effusion.DHT.KRPC
  doctest Effusion.DHT


  setup do
    port = Application.fetch_env!(:effusion_dht, :port)
    %{port: port}
  end

  test "responds to ping", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    :ok = :gen_udp.send(socket, 'localhost', port, ping())

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 1_000)
    response = KRPC.decode!(data)

    assert response["y"] == "r"
    assert response["r"] == %{"sender_id" => DHT.local_node_id()}
  end

  defp ping do
    KRPC.generate_transaction_id()
    |> KRPC.new_query("ping", %{sender_id: DHT.generate_node_id()})
    |> KRPC.encode!()
  end
end
