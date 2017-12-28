alias Effusion.Messages.Handshake

defmodule Effusion.PeerConnectionTest do
  use ExUnit.Case
  doctest Effusion.PeerConnection
  @moduletag :capture_log

  @local_peer_id <<0 :: size(160)>>
  @remote_peer_id <<1 :: size(160)>>
  @good_info_hash <<0 :: size(160)>>
  @bad_info_hash <<1 :: size(160)>>

  setup do
    opts = [:binary, packet: 0, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4040, opts)
    %{socket: socket}
  end

  test "responds with handshake", %{socket: socket} do
    request = Handshake.encode(@good_info_hash, @remote_peer_id)
    response = Handshake.encode(@good_info_hash, @local_peer_id)

    assert send_and_recv(socket, request) == response
  end

  test "closes connection when protocol name is wrong", %{socket: socket} do
    bad_request = bad_handshake(@good_info_hash, @remote_peer_id)

    :ok = :gen_tcp.send(socket, bad_request)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end

  test "closes connection when info hash isn't recognized", %{socket: socket} do
    bad_request = Handshake.encode(@bad_info_hash, @remote_peer_id)

    :ok = :gen_tcp.send(socket, bad_request)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end


    test "closes connection when peer ID matches our own", %{socket: socket} do
      bad_request = Handshake.encode(@good_info_hash, @local_peer_id)

      :ok = :gen_tcp.send(socket, bad_request)
      {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
    end

  defp bad_handshake(info_hash, peer_id) do
    bad_name_size = <<22>> ## should be 19!
    name = "BitTorrent protocol v2"
    reserved = <<0 :: size(64)>>

    bad_name_size <> name <> reserved <> info_hash <> peer_id
  end

  defp send_and_recv(socket, text) do
    :ok = :gen_tcp.send(socket, text)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
