defmodule Effusion.TCPSocketTest do
  use ExUnit.Case, async: true
  alias Effusion.TCPSocket
  alias Effusion.Messages
  doctest Effusion.TCPSocket

  @info_hash TestHelper.mint_info_hash()
  @local_peer_id "Effusion Experiment!"
  @host {127, 0, 0, 1}
  @port 5679

  @remote_peer_id "Other peer 123456789"

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000)

    on_exit(fn ->
      :ok = :gen_tcp.close(lsock)
    end)

    %{lsock: lsock, peer: %{address: {@host, @port}, peer_id: @remote_peer_id}}
  end

  describe "connect/1" do
    test "connects and sends a handshake first", %{lsock: lsock, peer: peer} do
      _ =
        Task.async(fn ->
          TCPSocket.connect(peer.address, @info_hash, @local_peer_id, peer.peer_id, [])
        end)

      {:ok, sock} = :gen_tcp.accept(lsock)
      {:ok, outgoing_handshake_bin} = :gen_tcp.recv(sock, 68)

      {:ok, outgoing_handshake} =
        outgoing_handshake_bin
        |> IO.iodata_to_binary()
        |> Messages.decode()

      assert {:handshake, @local_peer_id, @info_hash, _extensions} = outgoing_handshake
    end

    test "returns the socket and peer if the handshake succeeds", %{lsock: lsock, peer: peer} do
      task =
        Task.async(fn ->
          TCPSocket.connect(peer.address, @info_hash, @local_peer_id, peer.peer_id, [])
        end)

      {:ok, sock} = :gen_tcp.accept(lsock)
      {:ok, _} = :gen_tcp.recv(sock, 68)
      {:ok, good_handshake} = Messages.encode({:handshake, @remote_peer_id, @info_hash, []})
      :ok = :gen_tcp.send(sock, good_handshake)

      {:ok, _, remote_peer_id, []} = Task.await(task)
      assert remote_peer_id == @remote_peer_id
    end

    test "returns an error if the handshake is bad", %{lsock: lsock, peer: peer} do
      task =
        Task.async(fn ->
          TCPSocket.connect(peer.address, @info_hash, @local_peer_id, peer.peer_id, [])
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
