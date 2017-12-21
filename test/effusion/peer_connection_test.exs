defmodule Effusion.PeerConnectionTest do
  use ExUnit.Case
  doctest Effusion.PeerConnection
  @moduletag :capture_log

  setup do
    opts = [:binary, packet: 0, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4040, opts)
    %{socket: socket}
  end

  test "server interaction", %{socket: socket} do
    assert send_and_recv(socket, handshake()) == handshake()
  end

  test "closes connection when protocol name is wrong", %{socket: socket} do
    bad_name_size = <<22>> ## should be 19!
    name = "BitTorrent protocol v2"
    reserved = <<0 :: size(64)>>
    info_hash = <<0 :: size(160)>>
    peer_id = <<0 :: size(160)>>

    bad_handshake = bad_name_size <> name <> reserved <> info_hash <> peer_id

    :ok = :gen_tcp.send(socket, bad_handshake)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end

  test "closes connection when info hash isn't recognized", %{socket: socket} do
    bad_name_size = <<19>>
    name = "BitTorrent protocol"
    reserved = <<0 :: size(64)>>
    info_hash = <<1 :: size(160)>>
    peer_id = <<0 :: size(160)>>

    bad_handshake = bad_name_size <> name <> reserved <> info_hash <> peer_id

    :ok = :gen_tcp.send(socket, bad_handshake)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end

  defp handshake do
    name_size = <<19>>
    name = "BitTorrent protocol"
    reserved = <<0 :: size(64)>>
    info_hash = <<0 :: size(160)>>
    peer_id = <<0 :: size(160)>>

    name_size <> name <> reserved <> info_hash <> peer_id
  end

  defp send_and_recv(socket, text) do
    :ok = :gen_tcp.send(socket, text)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
