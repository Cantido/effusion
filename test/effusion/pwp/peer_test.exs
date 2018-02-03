defmodule Effusion.PWP.PeerTest do
  use ExUnit.Case
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Peer

  doctest Effusion.PWP.Peer

  @remote_host {127, 0, 0, 1}
  @port 5679
  @remote_address {@remote_host, @port}
  @local_peer_id "Effusion Experiment!"
  @remote_peer_id "Other peer 123456789"
  @info_hash TestHelper.mint_info_hash()

  @local_handshake Handshake.encode(@local_peer_id, @info_hash)
  @remote_handshake Handshake.encode(@remote_peer_id, @info_hash)

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000)

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  test "Peer performs handshake", %{lsock: lsock} do
    # XXX: In this test, we are acting as the "remote" peer.
    # So, technically, we are "remote", and we are sending stuff to "local".

      with {:ok, _pid} <- Peer.connect(@remote_address, @local_peer_id, @info_hash),
           {:ok, sock} <- :gen_tcp.accept(lsock, 5_000),
           {:ok, handshake_packet} <- :gen_tcp.recv(sock, 68),
           :ok <- :gen_tcp.send(sock, @remote_handshake),
           :ok <- :gen_tcp.close(sock)
      do
        handshake =
          handshake_packet
          |> IO.iodata_to_binary()
          |> Handshake.decode()
        assert {:ok, {@local_peer_id, @info_hash, _}} = handshake
      end
  end

end
