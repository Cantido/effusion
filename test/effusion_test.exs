defmodule EffusionTest do
  use ExUnit.Case
  alias Effusion.TCPSocket
  require Logger
  import Mox
  doctest Effusion

  setup :set_mox_global
  setup :verify_on_exit!

  @torrent TestHelper.tiny_meta()
  @multi_file_torrent TestHelper.multi_file_meta()
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
    Application.put_env(:effusion, :server_address, {{127, 0, 0, 1}, @local_port})
  end

  @tag :tmp_dir
  test "download a single-file torrent", %{lsock: lsock, tmp_dir: file} do
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])

    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Application.put_env(:effusion, :download_destination, file)

    Effusion.MockHTTP
    |> expect(:announce, 2, &stub_tracker/1)

    {:ok, _pid} = Effusion.start_download(@torrent)

    on_exit(fn ->
      :ok = Effusion.stop_download(@torrent.info_hash)
    end)

    test_pid = self()
    test_ref = make_ref()
    Solvent.subscribe(fn _, event_id, _ ->
        {:ok, event} = Solvent.EventStore.fetch(event_id)
        if event.subject == @torrent.info_hash do
          send(test_pid, test_ref)
        end
      end,
      types: ["io.github.cantido.effusion.torrent_completed"]
    )

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
    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = TCPSocket.recv(sock)

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = TCPSocket.recv(sock)
    {:ok, {:request, %{index: i2}}} = TCPSocket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:piece, 0, 0, "tin"})
    {:ok, m1} = TCPSocket.recv(sock)
    {:ok, m2} = TCPSocket.recv(sock)

    if elem(m1, 0) == :cancel do
      assert m1 == {:cancel, %{index: 0, offset: 0, size: 3}}
      assert m2 == {:have, 0}
    else
      assert m1 == {:have, 0}
      assert m2 == {:cancel, %{index: 0, offset: 0, size: 3}}
    end

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:piece, 1, 0, "y\n"})
    {:ok, m3} = TCPSocket.recv(sock)
    {:ok, m4} = TCPSocket.recv(sock)

    if elem(m3, 0) == :cancel do
      assert m3 == {:cancel, %{index: 1, offset: 0, size: 2}}
      assert m4 == {:have, 1}
    else
      assert m3 == {:have, 1}
      assert m4 == {:cancel, %{index: 1, offset: 0, size: 2}}
    end

    Effusion.pause_download(@torrent.info_hash)

    assert_receive ^test_ref

    assert {5, 5} == Effusion.progress(@torrent.info_hash)

    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents
  end

  @tag :tmp_dir
  test "download a multi-file torrent", %{lsock: lsock, tmp_dir: file} do
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])

    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Application.put_env(:effusion, :download_destination, file)

    Effusion.MockHTTP
    |> expect(:announce, 2, &stub_tracker/1)

    {:ok, _pid} = Effusion.start_download(@multi_file_torrent)

    {:ok, sock, _remote_peer, []} =
      TCPSocket.accept(
        lsock,
        @multi_file_torrent.info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )

    on_exit(fn ->
      TCPSocket.close(sock)
    end)

    {:ok, {:bitfield, remote_bitfield}} = TCPSocket.recv(sock)
    assert IntSet.new(remote_bitfield) |> Enum.empty?()

    bitfield = IntSet.new([0]) |> IntSet.bitstring()
    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = TCPSocket.recv(sock)

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = TCPSocket.recv(sock)

    assert i1 == 0

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:piece, 0, 0, "Hello\nworld!\n"})

    {:ok, {:cancel, %{index: 0, offset: 0, size: 13}}} = TCPSocket.recv(sock)
    {:ok, {:have, r1}} = TCPSocket.recv(sock)

    assert r1 == 0

    Process.sleep(100)

    Effusion.pause_download(@multi_file_torrent.info_hash)

    :file.datasync(file)

    {:ok, first_file_contents} = File.read(Path.join([file, "hello_world", "hello.txt"]))
    assert "Hello\n" == first_file_contents

    {:ok, second_file_contents} = File.read(Path.join([file, "hello_world", "world.txt"]))
    assert "world!\n" == second_file_contents
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

  @tag :tmp_dir
  test "receive a connection from a peer", %{tmp_dir: file} do
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])

    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Application.put_env(:effusion, :download_destination, file)

    # Expect started and completed message.
    # No "stopped" message, that's only if you stop downloading.
    Effusion.MockHTTP
    |> expect(:announce, 2, &stub_tracker_no_peers/1)

    {:ok, _pid} = Effusion.start_download(@torrent)

    on_exit(fn ->
      :ok = Effusion.stop_download(@torrent.info_hash)
    end)

    test_pid = self()
    test_ref = make_ref()
    Solvent.subscribe(fn _, event_id, _ ->
        {:ok, event} = Solvent.EventStore.fetch(event_id)
        if event.subject == @torrent.info_hash do
          send(test_pid, test_ref)
        end
      end,
      types: ["io.github.cantido.effusion.torrent_completed"]
    )

    {:ok, sock, _remote_peer, _ext} =
      TCPSocket.connect(
        {{127, 0, 0, 1}, @local_port},
        @torrent.info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )

    on_exit(fn ->
      TCPSocket.close(sock)
    end)

    {:ok, {:bitfield, <<0>>}} = TCPSocket.recv(sock)

    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring(byte_align: true)
    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = TCPSocket.recv(sock)

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, :unchoke)
    {:ok, {:request, %{index: i1}}} = TCPSocket.recv(sock)
    {:ok, {:request, %{index: i2}}} = TCPSocket.recv(sock)

    if i1 == 0 do
      assert i2 == 1
    else
      assert i1 == 1
      assert i2 == 0
    end

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:piece, 0, 0, "tin"})
    {:ok, m1} = TCPSocket.recv(sock)
    {:ok, m2} = TCPSocket.recv(sock)

    if elem(m1, 0) == :cancel do
      assert m1 == {:cancel, %{index: 0, offset: 0, size: 3}}
      assert m2 == {:have, 0}
    else
      assert m1 == {:have, 0}
      assert m2 == {:cancel, %{index: 0, offset: 0, size: 3}}
    end

    {:ok, _bytes_count} = TCPSocket.send_msg(sock, {:piece, 1, 0, "y\n"})
    {:ok, m3} = TCPSocket.recv(sock)
    {:ok, m4} = TCPSocket.recv(sock)

    if elem(m3, 0) == :cancel do
      assert m3 == {:cancel, %{index: 1, offset: 0, size: 2}}
      assert m4 == {:have, 1}
    else
      assert m3 == {:have, 1}
      assert m4 == {:cancel, %{index: 1, offset: 0, size: 2}}
    end

    Effusion.pause_download(@torrent.info_hash)

    assert_receive ^test_ref

    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents
  end
end
