defmodule EffusionTest do
  use ExUnit.Case
  alias Effusion.TCPSocket
  require Logger
  import Mox
  doctest Effusion

  setup :set_mox_global
  setup :verify_on_exit!

  @localhost {127, 0, 0, 1}

  @torrent TestHelper.tiny_meta()
  @local_port Application.fetch_env!(:effusion, :port)
  @remote_port 8002

  @remote_peer %{address: {{127, 0, 0, 1}, @remote_port}, peer_id: "Effusion Experiment!"}
  @local_peer_id "Fake-Remote-Peer----"

  defp stub_tracker(_) do
    {host, port} = @remote_peer.address

    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: host, port: port, peer_id: @local_peer_id}]
      }
    }
  end

  defp stub_tracker_no_peers(_) do
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
    {:ok, lsock} = TCPSocket.listen(port)

    on_exit(fn ->
      :ok = TCPSocket.close(lsock)
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
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])

    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Application.put_env(:effusion, :download_destination, file)

    Effusion.MockHTTP
    |> expect(:announce, 2, &stub_tracker/1)

    {:ok, _pid} = Effusion.start_download(@torrent)

    {:ok, sock, _remote_peer, []} =
      TCPSocket.accept(
        lsock,
        @torrent.info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )

    on_exit(fn ->
      TCPSocket.close(sock)
    end)

    {:ok, {:bitfield, remote_bitfield}} = TCPSocket.recv(sock)
    assert IntSet.new(remote_bitfield) |> Enum.empty?()

    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring()
    :ok = TCPSocket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = TCPSocket.recv(sock)

    :ok = TCPSocket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = TCPSocket.recv(sock)
    {:ok, {:request, %{index: i2}}} = TCPSocket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    :ok = TCPSocket.send_msg(sock, {:piece, 0, 0, "tin"})
    :ok = TCPSocket.send_msg(sock, {:piece, 1, 0, "y\n"})

    {:ok, {:have, r1}} = TCPSocket.recv(sock)
    {:ok, {:have, r2}} = TCPSocket.recv(sock)

    if r1 == 0 do
      assert r2 == 1
    else
      assert r1 == 1
      assert r2 == 0
    end

    Effusion.stop_download(@torrent.info_hash)

    Process.sleep(100)

    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents
  end

  # test "download a file from a peer supporting the fast extension", %{lsock: lsock, destfile: file} do
  #   old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
  #   Application.put_env(:effusion, :enabled_extensions, [:fast])
  #   on_exit(fn ->
  #     Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
  #   end)
  #   Application.put_env(:effusion, :download_destination, file)
  #
  #   # Expect started, completed, and stopped messages
  #   Effusion.THP.Mock
  #   |> expect(:announce, 3, &stub_tracker/9)
  #
  #   :ok = Effusion.start_download(@torrent)
  #
  #   {:ok, sock, _remote_peer, [:fast]} =
  #     Socket.accept(
  #       lsock,
  #       @info_hash,
  #       @local_peer_id,
  #       @remote_peer.peer_id,
  #       [:fast]
  #     )
  #
  #   on_exit(fn ->
  #     Socket.close(sock)
  #   end)
  #
  #   :ok = Socket.send_msg(sock, :have_all)
  #   {:ok, :interested} = Socket.recv(sock)
  #
  #   :ok = Socket.send_msg(sock, :unchoke)
  #   {:ok, {:request, %{index: i1}}} = Socket.recv(sock)
  #   {:ok, {:request, %{index: i2}}} = Socket.recv(sock)
  #
  #   if i1 == 0 do
  #     assert i2 == 1
  #   else
  #     assert i1 == 1
  #     assert i2 == 0
  #   end
  #
  #   :ok = Socket.send_msg(sock, {:piece, 0, 0, "tin"})
  #   :ok = Socket.send_msg(sock, {:piece, 1, 0, "y\n"})
  #
  #   {:ok, {:have, r1}} = Socket.recv(sock)
  #   {:ok, {:have, r2}} = Socket.recv(sock)
  #
  #   if r1 == 0 do
  #     assert r2 == 1
  #   else
  #     assert r1 == 1
  #     assert r2 == 0
  #   end
  #
  #   Process.sleep(500)
  #   :file.datasync(file)
  #
  #   {:ok, contents} = File.read(Path.join(file, "tiny.txt"))
  #
  #   assert "tiny\n" == contents
  #
  #   Process.sleep(200)
  # end

  # test "receive a connection from a peer", %{destfile: file} do
  #   old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
  #   Application.put_env(:effusion, :enabled_extensions, [])

  #   on_exit(fn ->
  #     Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
  #   end)

  #   Application.put_env(:effusion, :download_destination, file)

  #   # Expect started and completed message.
  #   # No "stopped" message, that's only if you stop downloading.
  #   Effusion.THP.Mock
  #   |> expect(:announce, 2, &stub_tracker_no_peers/9)

  #   :ok = Effusion.start_download(@torrent)

  #   {:ok, sock, _remote_peer, _ext} =
  #     TCPSocket.connect(
  #       {{127, 0, 0, 1}, @local_port},
  #       @info_hash,
  #       @local_peer_id,
  #       @remote_peer.peer_id,
  #       []
  #     )

  #   on_exit(fn ->
  #     TCPSocket.close(sock)
  #   end)

  #   {:ok, {:bitfield, <<0>>}} = TCPSocket.recv(sock)

  #   bitfield = IntSet.new([0, 1]) |> IntSet.bitstring(byte_align: true)
  #   :ok = TCPSocket.send_msg(sock, {:bitfield, bitfield})
  #   {:ok, :interested} = TCPSocket.recv(sock)

  #   :ok = TCPSocket.send_msg(sock, :unchoke)
  #   {:ok, {:request, %{index: i1}}} = TCPSocket.recv(sock)
  #   {:ok, {:request, %{index: i2}}} = TCPSocket.recv(sock)

  #   if i1 == 0 do
  #     assert i2 == 1
  #   else
  #     assert i1 == 1
  #     assert i2 == 0
  #   end

  #   :ok = TCPSocket.send_msg(sock, {:piece, 0, 0, "tin"})
  #   :ok = TCPSocket.send_msg(sock, {:piece, 1, 0, "y\n"})

  #   {:ok, {:have, r1}} = TCPSocket.recv(sock)
  #   {:ok, {:have, r2}} = TCPSocket.recv(sock)

  #   if r1 == 0 do
  #     assert r2 == 1
  #   else
  #     assert r1 == 1
  #     assert r2 == 0
  #   end

  #   Effusion.stop_download(@info_hash)
  #   :file.datasync(file)

  #   {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

  #   assert "tiny\n" == contents
  # end
end
