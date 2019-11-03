defmodule Effusion.BTP.DownloadTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Download
  alias Effusion.BTP.Swarm
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Block
  alias Effusion.BTP.Pieces
  doctest Effusion.BTP.Download
  import Mox

  setup :verify_on_exit!

  @torrent TestHelper.tiny_meta()

  setup do
    Effusion.BTP.Metainfo.Directory.insert(@torrent)
  end

  def peer do
    Peer.new(
      {{192, 168, 1, 2}, 8000},
      "local peer id ~~~~~~",
      @torrent.info_hash
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
    swarm = Effusion.BTP.Swarm.new("Effusion Experiment!", "Effusion Experiment!")
    |> Swarm.add(
      [
        %{
          ip: {192, 168, 1, 2},
          port: 8000,
          peer_id: "Fake Peer Id ~~~~~~~"
        }
      ]
    )

    Download.new(@torrent, {{192, 168, 1, 1}, 8080}, destfile)
    |> Map.put(:swarm, swarm)
  end

  def stub_tracker_response, do: %{interval: 9_000, peers: []}

  def stub_tracker(_, _, _, _, _, _, _, _, _, _) do
    {:ok, stub_tracker_response()}
  end

  test "expresses interest and unchokes after we send a bitfield", %{destfile: file} do
    peer = peer()
    session = new(file)

    {session, msgs} = Download.handle_message(session, peer.remote_peer_id, {:bitfield, <<0>>})

    assert msgs == [
      {:btp_send, "Fake Peer Id ~~~~~~~", :interested},
      {:btp_send, "Fake Peer Id ~~~~~~~", :unchoke}
    ]

    peer = Swarm.peer_for_address(session.swarm, peer.address)

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
    peer = peer()

    {session, _msg} =
      new(file)
      |> Download.handle_message(
        peer.remote_peer_id,
        {:piece, %{index: 0, offset: 0, data: "tin"}}
      )

    assert Pieces.bytes_completed(session.pieces) == 3

    params = Download.announce_params(session, :interval)
    assert [_, _, _, _, _, _, 3, 2, _, _] = params
  end

  test "includes peer_id in successive announcements", %{destfile: file} do
    session = new(file)
    session = Download.handle_tracker_response(session, %{tracker_id: "this is my tracker id", interval: 90_000, peers: []})
    response = Download.announce_params(session, :interval)

    assert [_, _, _, _, _, _, _, _, _, "this is my tracker id"] = response
  end

  test "sends HAVE messages once it finishes a piece", %{destfile: file} do
    peer = peer()

    {_session, msgs} =
      new(file)
      |> Download.handle_message(
        peer.remote_peer_id,
        {:piece, %{index: 0, offset: 0, data: "tin"}}
      )

    assert Enum.member?(msgs, {:broadcast, {:have, 0}})
  end

  test "returns :announce message on start", %{destfile: file} do
    {_dl, messages} = Download.start(new(file))
    assert [{:announce, _params}] = messages
  end

  test "sends CANCEL messages to peers it sent requests to", %{destfile: file} do
    info_hash = @torrent.info_hash

    piece_requestee_id = "Fake Peer Id ~~~~~~~"
    piece_sender_id = "123456789012345678~2"
    bystander_id = "123456789012345678~3"

    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_sender_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, piece_requestee_id)
    {:ok, _pid} = Registry.register(ConnectionRegistry, info_hash, bystander_id)

    block_id = Block.id(0, 0, 1)
    block_data = Block.new(0, 0, "t")

    {_s, messages} = new(file)
    |> Download.mark_block_requested({piece_requestee_id, block_id})
    |> Download.handle_message(piece_sender_id, {:piece, block_data})

    assert messages == [{:btp_send, piece_requestee_id, {:cancel, block_id}}]
  end

  test "saves peers that result from announce", %{destfile: file} do
    tracker_response = %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
          ]
        }

    session =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.handle_tracker_response(tracker_response)

    peer = Swarm.peer_for_address(session.swarm, {{127, 0, 0, 1}, 8001})

    assert peer.remote_peer_id == "remote peer id~~~~~~"
  end


  test "does not save the same peer ID twice", %{destfile: file} do
    tracker_response = %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
          ]
        }

    session =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.handle_tracker_response(tracker_response)
      |> Download.handle_tracker_response(tracker_response)

    assert Enum.count(Swarm.peers(session.swarm)) == 1
  end

  test "compact peer response", %{destfile: file} do
    tracker_response = %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001}
          ]
        }

    session =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.handle_tracker_response(tracker_response)

    assert Enum.count(Swarm.peers(session.swarm)) == 1
  end

  test "rejects the same ip", %{destfile: file} do
    tracker_response = %{
          interval: 9_000,
          peers: [
            %{ip: {127, 0, 0, 1}, port: 8001}
          ]
        }

    session =
      Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
      |> Download.handle_tracker_response(tracker_response)

    assert Enum.count(Swarm.peers(session.swarm)) == 1
  end
end
