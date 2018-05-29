defmodule Effusion.PWP.PeerSocketTest do
  use ExUnit.Case
  alias Effusion.PWP.PeerSocket
  alias Effusion.PWP.Messages
  doctest Effusion.PWP.PeerSocket


  @info_hash TestHelper.mint_info_hash()
  @host {127, 0, 0, 1}
  @port 5679

  @remote_peer_id "Other peer 123456789"

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000)

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  test "sends handshake to the creating process", %{lsock: lsock} do
    {:ok, _} = start_supervised {PeerSocket, [self(), lsock]}

    {:ok, sock} = :gen_tcp.connect(@host, @port, [active: false], 1_000)
    handshake = Messages.encode({:handshake, @remote_peer_id, @info_hash})
    :ok = :gen_tcp.send(sock, handshake)

    assert_receive {:handshake, @remote_peer_id, @info_hash, _reserved}
  end

  test "sends PWP messages to the creating process", %{lsock: lsock} do
    {:ok, _} = start_supervised {PeerSocket, [self(), lsock]}

    {:ok, sock} = :gen_tcp.connect(@host, @port, [active: false], 1_000)

    handshake = Messages.encode({:handshake, @remote_peer_id, @info_hash})
    :ok = :gen_tcp.send(sock, handshake)
    :timer.sleep(10)

    :ok = :inet.setopts(sock, packet: 4)
    {:ok, unchoke} = Messages.encode(:unchoke)
    :ok = :gen_tcp.send(sock, unchoke)

    assert_receive :unchoke
  end
end
