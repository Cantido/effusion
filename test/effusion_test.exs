defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion
  alias Effusion.BTP.Peer
  alias Effusion.PWP.TCP.Socket
  import Mox
  require Logger

  setup :verify_on_exit!
  setup :set_mox_global

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
  end


  @local_port 8001
  @remote_port 8002

  @torrent %{
    announce: "http://localhost:6969/announce",
    created_by: "Lovingly hand-crafted, by Rosa <3",
    creation_date: 1_517_785_476,
    encoding: "UTF-8",
    info: %{
      :length => 5,
      :name => "tiny.txt",
      :piece_length => 3,
      # Pieces are "tin" and "y\n"
      :pieces => [
        <<242, 105, 25, 118, 134, 197, 108, 67, 163, 82, 84, 216, 119, 167, 25, 148, 192, 181,
          112, 48>>,
        <<144, 99, 169, 240, 224, 50, 182, 35, 148, 3, 183, 25, 203, 187, 165, 106, 196, 228, 228,
          95>>
      ],
      "private" => 0
    },
    info_hash:
      <<95, 189, 143, 1, 37, 56, 146, 40, 140, 78, 2, 250, 208, 144, 217, 10, 49, 7, 64, 28>>
  }

  @remote_peer Peer.new({{127, 0, 0, 1}, @remote_port})
  @local_peer_id "Fake-Remote-Peer----"
  @info_hash @torrent.info_hash

  defp stub_tracker(_, _, _, _, _, _, _, _, _) do
    {host, port} = @remote_peer.address

    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: host, port: port, peer_id: @local_peer_id}]
      }
    }
  end

  defp stub_tracker_no_peers(_, _, _, _, _, _, _, _, _) do
    {
      :ok,
      %{
        interval: 90_000,
        peers: []
      }
    }
  end

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  setup do
    Temp.track!()

    {:ok, file} = Temp.path()

    on_exit(fn ->
      File.rm_rf(file)
    end)

    %{destfile: file}
  end

  setup do
    Application.put_env(:effusion, :server_address, {{127, 0, 0, 1}, @local_port})
  end

  test "download a file", %{lsock: lsock, destfile: file} do
    Application.put_env(:effusion, :download_destination, file)

    # Expect started, completed, and stopped messages
    Effusion.THP.Mock
    |> expect(:announce, 3, &stub_tracker/9)

    {:ok, pid} = Effusion.start_download(@torrent)
    on_exit(fn ->
      Effusion.stop_download(@torrent.info_hash)
    end)

    {:ok, sock, _remote_peer, [:fast]} =
      Socket.accept(
        lsock,
        @info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )

    on_exit(fn ->
      Socket.close(sock)
    end)

    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring()
    :ok = Socket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = Socket.recv(sock)

    :ok = Socket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = Socket.recv(sock)
    {:ok, {:request, %{index: i2}}} = Socket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    :ok = Socket.send_msg(sock, {:piece, 0, 0, "tin"})
    :ok = Socket.send_msg(sock, {:piece, 1, 0, "y\n"})

    {:ok, {:have, r1}} = Socket.recv(sock)
    {:ok, {:have, r2}} = Socket.recv(sock)

    if r1 == 0 do
      assert r2 == 1
    else
      assert r1 == 1
      assert r2 == 0
    end

    Socket.close(sock)

    :timer.sleep(700)
    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents

    Effusion.stop_download(@info_hash)
  end

  test "download a file from a peer supporting the fast extension", %{lsock: lsock, destfile: file} do
    Application.put_env(:effusion, :download_destination, file)

    # Expect started, completed, and stopped messages
    Effusion.THP.Mock
    |> expect(:announce, 3, &stub_tracker/9)

    {:ok, pid} = Effusion.start_download(@torrent)
    on_exit(fn ->
      Effusion.stop_download(@torrent.info_hash)
    end)

    {:ok, sock, _remote_peer, [:fast]} =
      Socket.accept(
        lsock,
        @info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        [:fast]
      )

    on_exit(fn ->
      Socket.close(sock)
    end)

    :ok = Socket.send_msg(sock, :have_all)
    {:ok, :interested} = Socket.recv(sock)

    :ok = Socket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = Socket.recv(sock)
    {:ok, {:request, %{index: i2}}} = Socket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    :ok = Socket.send_msg(sock, {:piece, 0, 0, "tin"})
    :ok = Socket.send_msg(sock, {:piece, 1, 0, "y\n"})

    {:ok, {:have, r1}} = Socket.recv(sock)
    {:ok, {:have, r2}} = Socket.recv(sock)

    if r1 == 0 do
      assert r2 == 1
    else
      assert r1 == 1
      assert r2 == 0
    end

    Socket.close(sock)

    :timer.sleep(700)
    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents

    Effusion.stop_download(@info_hash)
  end

  test "receive a connection from a peer", %{destfile: file} do
    Application.put_env(:effusion, :download_destination, file)

    # Expect started, completed, and stopped messages
    Effusion.THP.Mock
    |> expect(:announce, 3, &stub_tracker_no_peers/9)

    {:ok, pid} = Effusion.start_download(@torrent)
    on_exit(fn ->
      Effusion.stop_download(@torrent.info_hash)
    end)

    {:ok, sock, _remote_peer, _ext} =
      Socket.connect(
        {{127, 0, 0, 1}, @local_port},
        @info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )

    on_exit(fn ->
      Socket.close(sock)
    end)

    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring()
    :ok = Socket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = Socket.recv(sock)

    :ok = Socket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = Socket.recv(sock)
    {:ok, {:request, %{index: i2}}} = Socket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    :ok = Socket.send_msg(sock, {:piece, 0, 0, "tin"})
    :ok = Socket.send_msg(sock, {:piece, 1, 0, "y\n"})

    {:ok, {:have, r1}} = Socket.recv(sock)
    {:ok, {:have, r2}} = Socket.recv(sock)

    if r1 == 0 do
      assert r2 == 1
    else
      assert r1 == 1
      assert r2 == 0
    end

    Socket.close(sock)

    :timer.sleep(700)
    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents

    Effusion.stop_download(@info_hash)
  end
end
