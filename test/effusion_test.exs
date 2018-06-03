defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion
  alias Effusion.PWP.Messages
  import Mox

  setup :verify_on_exit!
  setup :set_mox_global

  @info_hash TestHelper.mint_info_hash()
  @meta TestHelper.mint_meta()

  @local_ip {10, 0, 0, 5}
  @local_port 8999
  @local_peer {@local_ip, @local_port}
  @local_peer_id "Effusion Experiment!"

  @remote_host {127, 0, 0, 1}
  @remote_port 5679
  @remote_peer_id "Other peer 123456789"
  @remote_handshake {:handshake, @remote_peer_id, @info_hash}

  defp stub_tracker(url, ip, port, peer_id, info_hash, up, down, left) do
    assert url == "https://torrents.linuxmint.com/announce.php"
    assert ip == @local_ip
    assert port == @local_port
    assert peer_id == @local_peer_id
    assert info_hash == @info_hash
    assert up == 0
    assert down == 0
    assert left == @meta.info.length
    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: @remote_host, port: @remote_port, peer_id: @remote_peer_id}]
      }
    }
  end

  setup do
    {:ok, lsock} = :gen_tcp.listen(@remote_port, active: false, reuseaddr: true, send_timeout: 5_000)

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  setup do
    Application.put_env(:effusion, :server_address, @local_peer)
  end

  test "starting a download performs a handshake", %{lsock: lsock} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_tracker/8)

    {:ok, _pid} = Effusion.start_download("test/linuxmint-18.3-cinnamon-64bit.iso.torrent", nil)
    {:ok, sock} = :gen_tcp.accept(lsock, 5_000)
    {:ok, handshake_packet} = :gen_tcp.recv(sock, 68)
    {:ok, hsbin} = Messages.encode(@remote_handshake)
    :ok = :gen_tcp.send(sock, hsbin)
    :ok = :gen_tcp.close(sock)

    handshake =
      handshake_packet
      |> IO.iodata_to_binary()
      |> Messages.decode()

    assert {:ok, {:handshake, @local_peer_id, @info_hash, _}} = handshake
  end
end
