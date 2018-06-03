defmodule Effusion.Application.SessionServerTest do
  use ExUnit.Case
  doctest Effusion.Application.SessionServer
  alias Effusion.Application.SessionServer
  alias Effusion.PWP.Messages
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
  @remote_handshake Messages.encode({:handshake, @remote_peer_id, @info_hash})


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

  setup do
    # StringIO and :ram devices don't work with the calls to
    # pwrite that session uses, so we gotta make an actual file
    {:ok, file, file_path} = Temp.open "Effusion.SessionServerTest.tmp"

    on_exit fn ->
      File.rm file_path
    end

    %{destfile: file, path: file_path}
  end

  test "SessionServer connects to remote and handshakes", %{lsock: lsock, destfile: destfile} do
    Effusion.THP.Mock
    |> expect(:announce, &mock_announce/8)

    {:ok, _pid} = start_supervised {SessionServer, [@meta, @local_peer, destfile]}
    {:ok, sock} = :gen_tcp.accept(lsock, 5_000)
    {:ok, handshake_packet} = :gen_tcp.recv(sock, 68)
    :ok = :gen_tcp.send(sock, @remote_handshake)
    :ok = :gen_tcp.close(sock)

    handshake =
      handshake_packet
      |> IO.iodata_to_binary()
      |> Messages.decode()

    assert {:ok, {:handshake, @local_peer_id, @info_hash, _}} = handshake
  end

  defp stub_announce(_, _, _, _, _, _, _, _) do
    {:ok, %{interval: 9_000, peers: []}}
  end

  test "doesn't crash if there are no peers currently available" do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    {:ok, _pid} = start_supervised {SessionServer, [@meta, @local_peer]}
  end

  test "writes verified pieces to file", %{destfile: file, path: path} do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    hello_meta = TestHelper.hello_meta()

    {:ok, pid} = start_supervised {SessionServer, [hello_meta, @local_peer, file]}

    block = %{index: 0, offset: 0, data: "Hello world!\n"}
    SessionServer.block(pid, block)

    assert "Hello world!\n" == File.read!(path)
  end


  test "keeps track of blocks, writes them out when we have a full piece", %{destfile: file, path: path} do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    tiny_meta = TestHelper.tiny_meta()

    {:ok, pid} = start_supervised {SessionServer, [tiny_meta, @local_peer, file]}

    SessionServer.block(pid, %{index: 0, offset: 0, data: "t"})
    assert "" == File.read!(path)

    SessionServer.block(pid, %{index: 0, offset: 1, data: "i"})
    assert "" == File.read!(path)

    SessionServer.block(pid, %{index: 0, offset: 2, data: "n"})
    assert "tin" == File.read!(path)
  end

  test "writes blocks in the correct order", %{destfile: file, path: path} do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    tiny_meta = TestHelper.tiny_meta()

    {:ok, pid} = start_supervised {SessionServer, [tiny_meta, @local_peer, file]}

    SessionServer.block(pid, %{index: 1, offset: 0, data: "y"})
    SessionServer.block(pid, %{index: 1, offset: 1, data: "\n"})
    SessionServer.block(pid, %{index: 0, offset: 0, data: "t"})
    SessionServer.block(pid, %{index: 0, offset: 1, data: "i"})
    SessionServer.block(pid, %{index: 0, offset: 2, data: "n"})
    "tiny\n" = File.read!(path)
  end

  test "does not request pieces it already has", %{destfile: file} do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    tiny_meta = TestHelper.tiny_meta()

    {:ok, pid} = start_supervised {SessionServer, [tiny_meta, @local_peer, file]}

    SessionServer.block(pid, %{index: 0, offset: 0, data: "tin"})
    %{index: index, offset: offset, size: size} = SessionServer.next_request(pid)

    # There are only two pieces in the tiny torrent
    assert index == 1
    assert offset in 0..1
    assert size == 2 - offset
  end
end
