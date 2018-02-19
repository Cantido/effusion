defmodule Effusion.PWP.PeerSocketTest do
  use ExUnit.Case
  alias Effusion.PWP.PeerSocket
  alias Effusion.PWP.Messages
  doctest Effusion.PWP.PeerSocket


  @host {127, 0, 0, 1}
  @port 5679

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000, packet: 4)

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  test "sends messages to the creating process", %{lsock: lsock} do
    {:ok, _} = start_supervised {PeerSocket, [self(), lsock]}

    {:ok, sock} = :gen_tcp.connect(@host, @port, [active: false, packet: 4], 1_000)
    {:ok, unchoke} = Messages.encode(:unchoke)
    :ok = :gen_tcp.send(sock, unchoke)

    assert_receive :unchoke
  end
end
