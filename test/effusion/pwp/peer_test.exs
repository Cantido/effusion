defmodule Effusion.PWP.PeerTest do
  use ExUnit.Case
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Peer

  doctest Effusion.PWP.Peer

  @remote_host {127, 0, 0, 1}
  @remote_port 4040
  @local_peer_id "Effusion Experiment!"
  @remote_peer_id "Other peer 123456789"
  @info_hash TestHelper.mint_info_hash()

  @local_handshake Handshake.encode(@local_peer_id, @info_hash)
  @remote_handshake Handshake.encode(@remote_peer_id, @info_hash)

  test "pwp" do
    port = 5679
    {:ok, lsock} = :gen_tcp.listen(port, active: false)
    {:ok, conn} = Peer.connect({{127, 0, 0, 1}, port}, "Effusion Experiment!", <<0::160>>)

    {:ok, csock} = :gen_tcp.accept(lsock, 5_000)

    hs_bin = with {:ok, hsreq} <- :gen_tcp.recv(csock, 68)
                    do IO.iodata_to_binary(hsreq)
                    end

    {:ok, {"Effusion Experiment!", <<0::160>>, _}} = Handshake.decode(hs_bin)

    :gen_tcp.close(lsock)
  end

end
