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
    on_exit fn ->
      :gen_udp.close(socket)
    end

    txid = KRPC.generate_transaction_id()
    ping =
      txid
      |> KRPC.new_query("ping", %{sender_id: DHT.generate_node_id()})
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, ping)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"] == %{"sender_id" => DHT.local_node_id()}
  end

  test "response to get_peers", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          sender_id: DHT.generate_node_id(),
          info_hash: :crypto.strong_rand_bytes(20)
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["sender_id"] == DHT.local_node_id()
    assert not is_nil(response["r"]["token"])
    assert response["r"]["nodes"] == <<>>
  end

  test "response to find_node", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("find_node", %{
          sender_id: DHT.generate_node_id(),
          target: :crypto.strong_rand_bytes(20)
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["sender_id"] == DHT.local_node_id()
    assert response["r"]["nodes"] == <<>>
  end
end
