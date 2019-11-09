defmodule Effusion.PWP.SocketTest do
  use ExUnit.Case
  alias Effusion.PWP.Socket
  alias Effusion.BTP.Peer
  alias Effusion.PWP.Messages
  doctest Effusion.PWP.Socket

  @info_hash TestHelper.mint_info_hash()
  @local_peer_id "Effusion Experiment!"
  @host {127, 0, 0, 1}
  @port 5679

  @remote_peer_id "Other peer 123456789"

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000)

    peer = Peer.new({@host, @port})

    on_exit(fn ->
      :ok = :gen_tcp.close(lsock)
    end)

    %{lsock: lsock, peer: peer}
  end

  describe "connect/1" do
    test "connects and sends a handshake first", %{lsock: lsock, peer: peer} do
      _ =
        Task.async(fn ->
          Socket.connect(peer.address, @info_hash, @local_peer_id, peer.remote_peer_id)
        end)

      {:ok, sock} = :gen_tcp.accept(lsock)
      {:ok, outgoing_handshake_bin} = :gen_tcp.recv(sock, 68)

      {:ok, outgoing_handshake} =
        outgoing_handshake_bin
        |> IO.iodata_to_binary()
        |> Messages.decode()

      assert {:handshake, @local_peer_id, @info_hash, <<0, 0, 0, 0, 0, 0, 0, 0>>} =
               outgoing_handshake
    end

    test "returns the socket and peer if the handshake succeeds", %{lsock: lsock, peer: peer} do
      task =
        Task.async(fn ->
          Socket.connect(peer.address, @info_hash, @local_peer_id, peer.remote_peer_id)
        end)

      {:ok, sock} = :gen_tcp.accept(lsock)
      {:ok, _} = :gen_tcp.recv(sock, 68)
      {:ok, good_handshake} = Messages.encode({:handshake, @remote_peer_id, @info_hash})
      :ok = :gen_tcp.send(sock, good_handshake)

      {:ok, _, remote_peer_id} = Task.await(task)
      assert remote_peer_id == @remote_peer_id
    end

    test "returns an error if the handshake is bad", %{lsock: lsock, peer: peer} do
      task =
        Task.async(fn ->
          Socket.connect(peer.address, @info_hash, @local_peer_id, peer.remote_peer_id)
        end)

      {:ok, sock} = :gen_tcp.accept(lsock)
      {:ok, _} = :gen_tcp.recv(sock, 68)

      {:ok, bad_handshake} =
        Messages.encode({:handshake, @remote_peer_id, "Bad info hash ~~~~~~"})

      :ok = :gen_tcp.send(sock, bad_handshake)

      {:error, {:mismatched_info_hash, _info}} = Task.await(task)
    end
  end
end
