alias Effusion.Messages.Handshake

defmodule Effusion.PeerConnectionTest do
  use ExUnit.Case
  doctest Effusion.PeerConnection
  @moduletag :capture_log

  @local_peer_id <<0 :: size(160)>>
  @remote_peer_id <<19 :: size(160)>>
  @good_info_hash <<4 :: size(160)>>
  @bad_info_hash <<12 :: size(160)>>

  setup do
    opts = [:binary, packet: 0, active: false]
    {:ok, socket} = :gen_tcp.connect('localhost', 4040, opts)
    {:ok, _pid} = Registry.register(Effusion.TorrentRegistry, @good_info_hash, :ok)
    %{socket: socket}
  end

  test "responds with handshake", %{socket: socket} do
    request = Handshake.encode(@remote_peer_id, @good_info_hash)

    actual_response = send_and_recv(socket, request)
    {:ok, actual_peer_id, actual_info_hash, _reserved} = Handshake.decode(actual_response)

    assert actual_peer_id == @local_peer_id
    assert actual_info_hash == @good_info_hash
  end

  test "closes connection when protocol name is wrong", %{socket: socket} do
    bad_request = bad_handshake(@remote_peer_id, @good_info_hash)

    :ok = :gen_tcp.send(socket, bad_request)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end

  test "closes connection when info hash isn't recognized", %{socket: socket} do
    bad_request = Handshake.encode(@remote_peer_id, @bad_info_hash)

    :ok = :gen_tcp.send(socket, bad_request)
    {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
  end


    test "closes connection when peer ID matches our own", %{socket: socket} do
      bad_request = Handshake.encode(@local_peer_id, @good_info_hash)

      :ok = :gen_tcp.send(socket, bad_request)
      {:error, :closed} = :gen_tcp.recv(socket, 0, 1000)
    end

  defp bad_handshake(peer_id, info_hash) do
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
