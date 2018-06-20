defmodule Effusion.BTP.SessionTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Session
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  doctest Effusion.BTP.Session
  import Mox

  setup :verify_on_exit!

  @torrent TestHelper.tiny_meta()

  def peer do
    Peer.new(
      {{192, 168, 1, 2}, 8000},
      "local peer id ~~~~~~",
      @torrent.info_hash,
      self()
    )
    |> Map.put(:remote_peer_id, "Fake Peer Id ~~~~~~~")
  end

  def new do
    Session.new(@torrent, {{192, 168, 1, 1}, 8080})
    |> Session.add_connected_peer(peer())
  end

  def stub_tracker(_, _, _, _, _, _, _, _, _) do
    {:ok, %{interval: 9_000, peers: []}}
  end

  test "expresses interest and unchokes after we send a bitfield" do
    peer = peer()
    session = new()

    {session, msgs} = Session.handle_message(session, peer.remote_peer_id, {:bitfield, <<0>>})


    assert msgs == [:interested, :unchoke]

    peer = Map.get(session.connected_peers, peer.remote_peer_id)

    assert peer.am_interested
    refute peer.am_choking
  end

  test "sends torrent's download progress in announce" do
    stub_tracker = fn (_, _, _, _, _, _, downloaded, left, _) ->
      assert downloaded == 3
      assert left == 2
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    peer = peer()

    {session, _msg} = new()
    |> Session.handle_message(peer.remote_peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})

    Session.announce(session, Effusion.THP.Mock)
  end

  test "sends HAVE messages once it finishes a piece" do
    stub_tracker = fn (_, _, _, _, _, _, _, _, _) ->
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    peer = peer()
    remote_peer_id = peer.remote_peer_id

    {:ok, _pid} = Registry.register(ConnectionRegistry, peer.info_hash, peer.remote_peer_id)

    {_session, _msg} = new()
    |> Session.handle_message(peer.remote_peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})

    assert_receive({:btp_send, ^remote_peer_id, {:have, 0}})
  end

  test "sends STARTED message to tracker on start" do
    stub_tracker = fn (_, _, _, _, _, _, _, _, event) ->
      assert event == :started
      {:ok, %{interval: 9_000, peers: []}}
    end
    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    Session.start(new(), Effusion.THP.Mock)
  end

  test "sends CANCEL messages to peers it sent requests to" do
    Effusion.THP.Mock
    |> stub(:announce, &stub_tracker/9)

    info_hash = @torrent.info_hash

    piece_sender_id = "123456789012345678~1"
    piece_requestee_id = "123456789012345678~2"
    bystander_id = "123456789012345678~3"

    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_sender_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_requestee_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, bystander_id)

    block_id = Block.id(0, 0, 1)
    block_data = Block.new(0, 0, "t")

    requested_pieces = Map.new([
      {block_id, MapSet.new([piece_sender_id, piece_requestee_id])}
    ])

    new()
    |> Map.put(:requested_pieces, requested_pieces)
    |> Session.handle_message(piece_sender_id, {:piece, block_data})

    refute_received {:btp_send, ^piece_sender_id, {:cancel, ^block_id}}
    assert_received {:btp_send, ^piece_requestee_id, {:cancel, ^block_id}}
    refute_received {:btp_send, ^bystander_id, {:cancel, ^block_id}}
  end
end
