defmodule Effusion.PWP.PeerTest do
  use ExUnit.Case
  import Mox
  alias Effusion.PWP.Messages.Handshake

  doctest Effusion.PWP.Peer

  @remote_host {127, 0, 0, 1}
  @remote_port 4040
  @local_peer_id "Effusion Experiment!"
  @remote_peer_id "Other peer 123456789"
  @info_hash TestHelper.mint_info_hash()

  @local_handshake Handshake.encode(@local_peer_id, @info_hash)
  @remote_handshake Handshake.encode(@remote_peer_id, @info_hash)

  setup :verify_on_exit!
  setup :set_mox_global

  defp stub_connect(ip, port, _opts) do
    assert ip == @remote_host
    assert port == @remote_port
    {:ok, {}}
  end

  defp stub_tcp_send_handshake({}, sent) do
    assert sent == @local_handshake
    :ok
  end

  defp stub_tcp_recv_handshake({}, 68) do
    {:ok, @remote_handshake}
  end

  defp stub_tcp_recv_msg({}, 0) do
    {:ok, <<1>>}
  end

  # test "handshakes with other peer" do
  #   Effusion.Transport.Mock
  #     |> expect(:connect, &stub_connect/3)
  #     |> expect(:send, &stub_tcp_send_handshake/2)
  #     |> expect(:recv, &stub_tcp_recv_handshake/2)
  #     |> expect(:recv, &stub_tcp_recv_msg/2)
  #
  #   {:ok, _peer} = start_supervised {Effusion.PWP.Peer, [@remote_host, @remote_port, @local_peer_id, @info_hash]}
  #
  #   :timer.sleep(1)
  # end
end
