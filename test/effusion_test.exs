defmodule EffusionTest do
  use Effusion.EventStoreCase
  doctest Effusion
  alias Effusion.BTP.Peer
  alias Effusion.DHT
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.DHT.KRPC.{Query, Response}
  alias Effusion.PWP.TCP.Socket
  import Mox
  import Ecto.Query
  require Logger



  setup :verify_on_exit!
  setup :set_mox_global

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
  end

  @localhost {127, 0, 0, 1}

  @local_port Application.fetch_env!(:effusion, :port)
  @remote_port 8002

  @local_dht_port Application.fetch_env!(:effusion, :dht_port)
  @remote_dht_port 8007

  @torrent TestHelper.tiny_meta()

  @remote_peer %{address: {{127, 0, 0, 1},  @remote_port}, peer_id: "Effusion Experiment!"}
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
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])
    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Application.put_env(:effusion, :download_destination, file)

    # Expect started and completed message.
    # No "stopped" message, that's only if you stop downloading.
    Effusion.THP.Mock
    |> expect(:announce, 2, &stub_tracker/9)

    :ok = Effusion.start_download(@torrent)

    {:ok, sock, _remote_peer, []} =
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

    {:ok, {:bitfield, remote_bitfield}} = Socket.recv(sock)
    assert IntSet.new(remote_bitfield) |> Enum.empty?()

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

    Effusion.stop_download(@info_hash)
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

  test "receive a connection from a peer", %{destfile: file} do
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [])
    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)
    Application.put_env(:effusion, :download_destination, file)

    # Expect started and completed message.
    # No "stopped" message, that's only if you stop downloading.
    Effusion.THP.Mock
    |> expect(:announce, 2, &stub_tracker_no_peers/9)

    :ok = Effusion.start_download(@torrent)

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

    {:ok, {:bitfield, <<0>>}} = Socket.recv(sock)

    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring(byte_align: true)
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

    Effusion.stop_download(@info_hash)
    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents
  end

  # test "dht node" do
  #   old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
  #   Application.put_env(:effusion, :enabled_extensions, [:dht])
  #   on_exit(fn ->
  #     Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
  #   end)
  #
  #   Effusion.THP.Mock
  #   |> stub(:announce, &stub_tracker_no_peers/9)
  #
  #   :ok = Effusion.start_download(@torrent)
  #
  #   # The server will accept a DHT request, so let's send one
  #   {:ok, sock} = :gen_udp.open(@local_port + 5, active: :once)
  #   on_exit(fn ->
  #     :gen_udp.close(sock)
  #   end)
  #
  #   mock_node_id = DHT.node_id()
  #   mock_transaction_id = DHT.transaction_id()
  #   {:ok, request} = Query.encode({:ping, mock_transaction_id, mock_node_id}) |> Bento.encode()
  #
  #   :ok = :gen_udp.send(sock, {127, 0, 0, 1}, @local_port, request)
  #
  #   packet = receive do
  #     {:udp, _socket, _ip, _in_port_no, packet} -> packet
  #   after
  #     500 -> flunk "No KRPC response"
  #   end
  #
  #   {:ping, ^mock_transaction_id, _server_node_id} = Bento.decode!(packet) |> Response.decode()
  # end

  test "dht node reaches out for peers", %{lsock: lsock} do
    old_supported_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    Application.put_env(:effusion, :enabled_extensions, [:dht])
    on_exit(fn ->
      Application.put_env(:effusion, :enabled_extensions, old_supported_extensions)
    end)

    Effusion.THP.Mock
    |> stub(:announce, &stub_tracker_no_peers/9)

    {:ok, socket} = :gen_udp.open(@remote_dht_port, active: :once)

    :ok = Effusion.start_download(@torrent)

    primary_node_id = DHT.node_id()
    remote_node_id = DHT.node_id()

    :ok = Effusion.CQRS.Contexts.DHT.start_dht(primary_node_id)
    :ok = Effusion.CQRS.Contexts.DHT.add_node(
      primary_node_id,
      remote_node_id,
      {127, 0, 0, 1},
      @remote_dht_port
    )
    :ok = Effusion.CQRS.Contexts.DHT.enable_dht_for_download(
      @info_hash,
      primary_node_id
    )

    # Now the DHT node knows a remote node exists, and then DHT is enabled.
    # It should send a request to us for peers related to @info_hash

    packet = receive do
      {:udp, _socket, _ip, _in_port_no, packet} -> packet
    after
      500 -> flunk "No KRPC response"
    end

    assert {:get_peers, transaction_id, actual_node_id, actual_info_hash} = Bento.decode!(packet) |> Query.decode()
    assert actual_node_id == primary_node_id
    assert actual_info_hash == @info_hash
    assert not is_nil(transaction_id)
    assert transaction_id != "null"

    # Now let's respond with a peer and see if the app tries to connect
    # we're cheating a little since the host & port is the same as
    # the one that returned the data, but this app isn't smart enough to notice.

    packet =
      {
        :get_peers_matching,
        transaction_id,
        remote_node_id,
        DHT.token(),
        [{@localhost, @remote_port}]
      }
      |> Response.encode()
      |> Bento.encode!()

    {:ok, socket} = :gen_udp.open(0)
    :ok = :gen_udp.send(socket, @localhost, @local_dht_port, packet)
    :ok = :gen_udp.close(socket)

    {:ok, sock, _remote_peer, [:dht]} =
      Socket.accept(
        lsock,
        @info_hash,
        @local_peer_id,
        @remote_peer.peer_id,
        []
      )
  end
end
