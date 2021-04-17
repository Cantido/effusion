defmodule Effusion.DHTTest do
  use ExUnit.Case
  alias Effusion.DHT
  alias Effusion.DHT.KRPC
  import Bitwise
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

  test "get_peers responds with nodes when we don't have any peers", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    # generate 8 peers with node IDs very close to the target

    peers =
      Enum.map(1..8, fn distance ->
        id = generate_close_id(target, distance)
        {:ok, socket} = :gen_udp.open(0, [:binary, {:active, :false}])
        {:ok, port} = :inet.port(socket)

        %{
          id: id,
          socket: socket,
          port: port
        }
      end)

    # have those peers ping the server, so it remembers them

    Enum.each(peers, fn peer ->
      query =
        KRPC.generate_transaction_id()
        |> KRPC.new_query("ping", %{sender_id: peer.id})
        |> KRPC.encode!()

      :ok = :gen_udp.send(peer.socket, 'localhost', port, query)
      # read to clear the response from the socket
      {:ok, {_host, _port, _data}} = :gen_udp.recv(peer.socket, 0, 5_000)
      :ok = :gen_udp.close(peer.socket)
    end)

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          sender_id: DHT.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["sender_id"] == DHT.local_node_id()
    assert not is_nil(response["r"]["token"])

    Enum.map(peers, fn peer ->
      peer.id <> <<127, 0, 0, 1>> <> <<peer.port::integer-size(16)>>
    end)
    |> Enum.each(fn compacted_peer ->
      assert :binary.match(response["r"]["nodes"], compacted_peer) != :nomatch
    end)
  end

  test "find_node responds with closest nodes", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    # generate 8 peers with node IDs very close to the target

    close_peers =
      Enum.map(1..8, fn distance ->
        id = generate_close_id(target, distance)
        {:ok, socket} = :gen_udp.open(0, [:binary, {:active, :false}])
        {:ok, port} = :inet.port(socket)

        %{
          id: id,
          socket: socket,
          port: port
        }
      end)

    far_peers =
      Enum.map(1..8, fn _index ->
        id = :crypto.strong_rand_bytes(20)
        {:ok, socket} = :gen_udp.open(0, [:binary, {:active, :false}])
        {:ok, port} = :inet.port(socket)

        %{
          id: id,
          socket: socket,
          port: port
        }
      end)

    peers = close_peers ++ far_peers

    # have those peers ping the server, so it remembers them

    Enum.each(peers, fn peer ->
      query =
        KRPC.generate_transaction_id()
        |> KRPC.new_query("ping", %{sender_id: peer.id})
        |> KRPC.encode!()

      :ok = :gen_udp.send(peer.socket, 'localhost', port, query)
      # read to clear the response from the socket
      {:ok, {_host, _port, _data}} = :gen_udp.recv(peer.socket, 0, 5_000)
      :ok = :gen_udp.close(peer.socket)
    end)

    # Now we find_node to get those close peers

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("find_node", %{
          sender_id: DHT.generate_node_id(),
          target: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["sender_id"] == DHT.local_node_id()

    # Assert that the close peers show in the resposne

    Enum.map(close_peers, fn peer ->
      peer.id <> <<127, 0, 0, 1>> <> <<peer.port::integer-size(16)>>
    end)
    |> Enum.each(fn compacted_peer ->
      assert :binary.match(response["r"]["nodes"], compacted_peer) != :nomatch
    end)

    # Assert that the far-away peers are not in the response

    Enum.map(far_peers, fn peer ->
      peer.id <> <<127, 0, 0, 1>> <> <<peer.port::integer-size(16)>>
    end)
    |> Enum.each(fn compacted_peer ->
      assert :binary.match(response["r"]["nodes"], compacted_peer) == :nomatch
    end)
  end

  defp generate_close_id(target, distance) do
    id_int = bxor(:crypto.bytes_to_integer(target), distance)
    <<id_int::integer-size(160)>>
  end

  test "response to announce_peer with a bad token", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("announce_peer", %{
          sender_id: DHT.generate_node_id(),
          target: :crypto.strong_rand_bytes(20)
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "e"
    assert response["e"] == [203, "Bad token"]
  end
end
