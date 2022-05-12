defmodule Effusion.DHTTest do
  use ExUnit.Case
  alias Effusion.DHT
  alias Effusion.DHT.KRPC
  alias Effusion.DHT.Node
  import Bitwise
  doctest Effusion.DHT

  setup do
    port = Enum.random(1025..65535)
    node_id = Node.generate_node_id()

    {:ok, server} = start_supervised({Effusion.DHT.ServerManager, [node_id: node_id]})
    {:ok, _udp} = start_supervised({Effusion.DHT.UDPListener, [port: port, server: server]})

    %{port: port, node_id: node_id}
  end

  test "responds to ping", %{port: port, node_id: node_id} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    txid = KRPC.generate_transaction_id()
    ping =
      txid
      |> KRPC.new_query("ping", %{id: Node.generate_node_id()})
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, ping)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"] == %{"id" => node_id}
  end

  test "get_peers responds with nodes when we don't have any peers", %{port: port, node_id: node_id} do
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
        |> KRPC.new_query("ping", %{id: peer.id})
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
          id: Node.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["id"] == node_id
    assert not is_nil(response["r"]["token"])

    Enum.map(peers, fn peer ->
      peer.id <> <<127, 0, 0, 1>> <> <<peer.port::integer-size(16)>>
    end)
    |> Enum.each(fn compacted_peer ->
      assert :binary.match(response["r"]["nodes"], compacted_peer) != :nomatch
    end)
  end

  test "get_peers responds with peers when we know about the torrent", %{port: port, node_id: node_id} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    # generate the peer we'll respond with

    :ok = Effusion.add_peer(target, {127, 0, 0, 1}, 8080)

    # Ask the DHT for peers

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          id: Node.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["id"] == node_id
    assert not is_nil(response["r"]["token"])

    assert response["r"]["values"] == <<127, 0, 0, 1, 8080::integer-size(16)>>
  end

  test "find_node responds with closest nodes", %{port: port, node_id: node_id} do
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
        |> KRPC.new_query("ping", %{id: peer.id})
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
          id: Node.generate_node_id(),
          target: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "r"
    assert response["r"]["id"] == node_id

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

    target = :crypto.strong_rand_bytes(20)

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("announce_peer", %{
          id: Node.generate_node_id(),
          info_hash: target,
          port: 8080,
          implied_port: 0,
          token: DHT.generate_announce_peer_token()
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid
    assert response["y"] == "e"
    assert response["e"] == [203, "Bad token"]
  end

  test "response to announce_peer with a good token", %{port: port, node_id: node_id} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          id: Node.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    token = response["r"]["token"]

    peer_port = Enum.random(1025..65535)
    txid_announce_peer = KRPC.generate_transaction_id()
    announce_peer =
      txid_announce_peer
      |> KRPC.new_query("announce_peer", %{
          id: Node.generate_node_id(),
          info_hash: target,
          port: peer_port,
          implied_port: 0,
          token: token
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, announce_peer)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid_announce_peer
    assert response["y"] == "r"
    assert response["r"]["id"] == node_id

    Process.sleep(100)

    peers = Effusion.peers(target)

    assert Enum.any?(peers, fn {_host, port} -> port == peer_port end)
  end

  test "response to announce_peer with a good token with an implied port", %{port: port, node_id: node_id} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          id: Node.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    token = response["r"]["token"]

    txid_announce_peer = KRPC.generate_transaction_id()
    announce_peer =
      txid_announce_peer
      |> KRPC.new_query("announce_peer", %{
          id: Node.generate_node_id(),
          info_hash: target,
          port: Enum.random(1025..65535),
          implied_port: 1,
          token: token
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, announce_peer)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid_announce_peer
    assert response["y"] == "r"
    assert response["r"]["id"] == node_id

    Process.sleep(100)

    peers = Effusion.peers(target)

    {:ok, peer_port} = :inet.port(socket)

    assert Enum.any?(peers, fn {_host, port} -> port == peer_port end), "No peers with port #{peer_port} found"
  end

  test "response to announce_peer with a mismatched token", %{port: port} do
    {:ok, socket} = :gen_udp.open(0, [:binary, {:active, false}])
    on_exit fn ->
      :gen_udp.close(socket)
    end

    target = :crypto.strong_rand_bytes(20)

    txid = KRPC.generate_transaction_id()
    get_peers =
      txid
      |> KRPC.new_query("get_peers", %{
          id: Node.generate_node_id(),
          info_hash: target
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, get_peers)

    {:ok, {_host, _port, _data}} = :gen_udp.recv(socket, 0, 5_000)

    txid_announce_peer = KRPC.generate_transaction_id()
    announce_peer =
      txid_announce_peer
      |> KRPC.new_query("announce_peer", %{
          id: Node.generate_node_id(),
          info_hash: target,
          port: 8080,
          implied_port: 0,
          token: DHT.generate_announce_peer_token()
        })
      |> KRPC.encode!()

    :ok = :gen_udp.send(socket, 'localhost', port, announce_peer)

    {:ok, {_host, _port, data}} = :gen_udp.recv(socket, 0, 5_000)
    response = KRPC.decode!(data)

    assert response["t"] == txid_announce_peer
    assert response["y"] == "e"
    assert response["e"] == [203, "Bad token"]
  end
end
