defmodule Effusion.BTP.DownloadTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Download
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  alias Effusion.BTP.Torrent
  doctest Effusion.BTP.Download
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

  setup do
    Temp.track!()

    {:ok, file} = Temp.path()

    on_exit(fn ->
      File.rm_rf(file)
    end)

    %{destfile: file}
  end

  def new(destfile) do
    peer = peer()
    Download.new(@torrent, {{192, 168, 1, 1}, 8080}, destfile)
    |> Map.update(:peers, Map.new(), &Map.put(&1, peer.address, peer))
    |> Map.update(:peer_addresses, Map.new(), &Map.put(&1, "Fake Peer Id ~~~~~~~", {{192, 168, 1, 2}, 8000}))
  end

  def stub_tracker(_, _, _, _, _, _, _, _, _, _) do
    {:ok, %{interval: 9_000, peers: []}}
  end

  test "expresses interest and unchokes after we send a bitfield", %{destfile: file} do
    peer = peer()
    session = new(file)

    {session, msgs} = Download.handle_message(session, peer.remote_peer_id, {:bitfield, <<0>>})

    assert msgs == [
      {:btp_send, "Fake Peer Id ~~~~~~~", :interested},
      {:btp_send, "Fake Peer Id ~~~~~~~", :unchoke}
    ]

    peer = Map.get(session.peers, peer.address)

    assert peer.am_interested
    refute peer.am_choking
  end

  test "is not done when we don't have all the pieces", %{destfile: file} do
    peer = peer()
    session = new(file)

    {session, _msgs} =
      Download.handle_message(session, peer.remote_peer_id, {:piece, Block.new(0, 0, "tin")})

    refute Download.done?(session)
  end

  test "returns an error when we get a :have message for a piece outside the torrent's bounds", %{
    destfile: file
  } do
    peer = peer()
    session = new(file)

    assert {:error, :index_out_of_bounds} ==
             Download.handle_message(session, peer.remote_peer_id, {:have, 3})
  end

  test "returns an error when we get a :bitfield message for a piece outside the torrent's bounds",
       %{destfile: file} do
    peer = peer()
    session = new(file)

    assert {:error, :index_out_of_bounds} ==
             Download.handle_message(session, peer.remote_peer_id, {:bitfield, <<0b001>>})
  end

  test "is done when we have all the pieces", %{destfile: file} do
    peer = peer()
    session = new(file)

    {session, _msgs} =
      Download.handle_message(session, peer.remote_peer_id, {:piece, Block.new(0, 0, "tin")})

    {session, _msgs} =
      Download.handle_message(session, peer.remote_peer_id, {:piece, Block.new(1, 0, "y\n")})

    assert Download.done?(session)
  end

  test "sends torrent's download progress in announce", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, downloaded, left, _, _ ->
      assert downloaded == 3
      assert left == 2
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    peer = peer()

    {session, _msg} =
      new(file)
      |> Download.handle_message(
        peer.remote_peer_id,
        {:piece, %{index: 0, offset: 0, data: "tin"}}
      )

    assert Torrent.bytes_completed(session.torrent) == 3

    Download.announce(session, Effusion.THP.Mock)
  end

  test "includes peer_id in successive announcements", %{destfile: file} do
    stub_tracker_1 = fn _, _, _, _, _, _, _, _, _, _ ->
      {:ok, %{tracker_id: "this is my tracker id", interval: 9_000, peers: []}}
    end

    stub_tracker_2 = fn _, _, _, _, _, _, _, _, _, tracker_id ->
      assert tracker_id == "this is my tracker id"
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker_1)
    |> expect(:announce, 2, stub_tracker_2)

    session = new(file)
    {session, _res} = Download.announce(session, Effusion.THP.Mock, :started)
    {session, _res} = Download.announce(session, Effusion.THP.Mock, :interval)
    {_session, _res} = Download.announce(session, Effusion.THP.Mock, :completed)
  end

  test "sends HAVE messages once it finishes a piece", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, _, _ ->
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    peer = peer()

    {_session, msgs} =
      new(file)
      |> Download.handle_message(
        peer.remote_peer_id,
        {:piece, %{index: 0, offset: 0, data: "tin"}}
      )

    assert Enum.member?(msgs, {:broadcast, {:have, 0}})
  end

  test "sends STARTED message to tracker on start", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, event, _ ->
      assert event == :started
      {:ok, %{interval: 9_000, peers: []}}
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    Download.start(new(file), Effusion.THP.Mock)
  end

  test "sends CANCEL messages to peers it sent requests to", %{destfile: file} do
    Effusion.THP.Mock
    |> stub(:announce, &stub_tracker/10)

    info_hash = @torrent.info_hash

    piece_sender_id = "Fake Peer Id ~~~~~~~"
    piece_requestee_id = "123456789012345678~2"
    bystander_id = "123456789012345678~3"

    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_sender_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_requestee_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, bystander_id)

    block_id = Block.id(0, 0, 1)
    block_data = Block.new(0, 0, "t")

    requested_pieces =
      Map.new([
        {block_id, MapSet.new([piece_sender_id, piece_requestee_id])}
      ])

    {_s, messages} = new(file)
    |> Map.put(:requested_pieces, requested_pieces)
    |> Download.handle_message(piece_sender_id, {:piece, block_data})

    assert messages == [{:btp_send, piece_requestee_id, {:cancel, block_id}}]
  end

  test "saves peers that result from announce", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, _, _ ->
      {
        :ok,
        %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
          ]
        }
      }
    end

    Effusion.THP.Mock
    |> expect(:announce, stub_tracker)

    {session, _res} =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.announce(Effusion.THP.Mock, :started)

    peer = Map.get(session.peers, {{127, 0, 0, 1}, 8001})

    assert peer.remote_peer_id == "remote peer id~~~~~~"
  end


  test "does not save the same peer ID twice", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, _, _ ->
      {
        :ok,
        %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
          ]
        }
      }
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    {session, _res} =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.announce(Effusion.THP.Mock, :started)
    {session, _res} = Download.announce(session, Effusion.THP.Mock)

    assert Enum.count(session.peers) == 1
  end

  test "compact peer response", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, _, _ ->
      {
        :ok,
        %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001}
          ]
        }
      }
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    {session, _res} =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.announce(Effusion.THP.Mock, :started)
    {session, _res} = Download.announce(session, Effusion.THP.Mock)

    assert Enum.count(session.peers) == 1
  end

  test "rejects the same ip", %{destfile: file} do
    stub_tracker = fn _, _, _, _, _, _, _, _, _, _ ->
      {
        :ok,
        %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001}
          ]
        }
      }
    end

    Effusion.THP.Mock
    |> stub(:announce, stub_tracker)

    {session, _res} =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.announce(Effusion.THP.Mock, :started)
    {session, _res} = Download.announce(session, Effusion.THP.Mock)

    assert Enum.count(session.peers) == 1
  end
end