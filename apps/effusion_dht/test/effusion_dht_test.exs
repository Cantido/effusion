defmodule Effusion.DHTTest do
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
    :ok = Effusion.DHT.Nodes.add(
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
