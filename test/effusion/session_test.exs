defmodule Effusion.SessionTest do
  use ExUnit.Case
  doctest Effusion.Session
  alias Effusion.Session
  alias Effusion.PWP.Messages.Handshake
  import Mox

  setup :verify_on_exit!
  setup :set_mox_global

  @info_hash TestHelper.mint_info_hash()
  @meta TestHelper.mint_meta()
  @left 1899528192

  @local_ip {10, 0, 0, 5}
  @local_port 8999
  @local_peer {@local_ip, @local_port}
  @local_peer_id "Effusion Experiment!"

  @remote_host {127, 0, 0, 1}
  @remote_port 5679
  @remote_peer_id "Other peer 123456789"
  @remote_handshake Handshake.encode(@remote_peer_id, @info_hash)


  defp mock_announce(url, ip, port, peer_id, info_hash, up, down, left) do
    assert url == "https://torrents.linuxmint.com/announce.php"
    assert ip == @local_ip
    assert port == @local_port
    assert peer_id == @local_peer_id
    assert info_hash == @info_hash
    assert up == 0
    assert down == 0
    assert left == @left
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

  test "Session connects to remote and handshakes", %{lsock: lsock} do
    Effusion.THP.Mock
    |> expect(:announce, &mock_announce/8)

    {:ok, _pid} = start_supervised {Session, [@meta, @local_peer]}
    {:ok, sock} = :gen_tcp.accept(lsock, 5_000)
    {:ok, handshake_packet} = :gen_tcp.recv(sock, 68)
    :ok = :gen_tcp.send(sock, @remote_handshake)
    :ok = :gen_tcp.close(sock)

    handshake =
      handshake_packet
      |> IO.iodata_to_binary()
      |> Handshake.decode()

    assert {:ok, {@local_peer_id, @info_hash, _}} = handshake
  end

  defp stub_announce(_, _, _, _, _, _, _, _) do
    {:ok, %{interval: 9_000, peers: []}}
  end

  test "doesn't crash if there are no peers currently available" do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    {:ok, _pid} = start_supervised {Session, [@meta, @local_peer]}
  end

  test "writes verified pieces to file" do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    hello_meta = TestHelper.hello_meta()

    {:ok, file} = StringIO.open("")

    {:ok, pid} = start_supervised {Session, [hello_meta, @local_peer, file]}

    block = %{index: 0, offset: 0, data: "Hello world!\n"}
    Session.block(pid, block)

    {:ok, {"", "Hello world!\n"}} = StringIO.close(file)
  end
end
