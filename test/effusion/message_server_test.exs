defmodule Effusion.MessageServerTest do
  use ExUnit.Case
  doctest Effusion.MessageServer
  @moduletag :capture_log

  setup do
    opts = [:binary, packet: 0, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4040, opts)
    %{socket: socket}
  end

  test "server interaction", %{socket: socket} do
    assert send_and_recv(socket, handshake()) == handshake()
  end

  test "closes connection when given a bad handshake", %{socket: socket} do
    bad_name_size = <<18>> ## should be 19!
    name = "BitTorrent protocol"
    reserved = <<0 :: size(64)>>
    info_hash = <<0 :: size(160)>>
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

  defp send_and_recv(socket, command) do
    :ok = :gen_tcp.send(socket, command)
    {:ok, data} = :gen_tcp.recv(socket, 0, 1000)
    data
  end
end
