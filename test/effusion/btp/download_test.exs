defmodule Effusion.BTP.DownloadTest do
  use ExUnit.Case
  alias Effusion.BTP.Download
  alias Effusion.BTP.Swarm
  alias Effusion.BTP.Peer
  doctest Effusion.BTP.Download
  import Mox

  setup :verify_on_exit!

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
    :ok
  end


  @torrent TestHelper.tiny_meta()

  setup do
    Effusion.BTP.Metainfo.Directory.insert(@torrent)
  end

  def peer do
    Peer.new({{192, 168, 1, 2}, 8000}, "Fake Peer Id ~~~~~~~")
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
    swarm =
      Effusion.BTP.Swarm.new("Effusion Experiment!", "Effusion Experiment!")
      |> Swarm.add([
        %{
          ip: {192, 168, 1, 2},
          port: 8000,
          peer_id: "Fake Peer Id ~~~~~~~"
        }
      ])

    Download.new(@torrent, {{192, 168, 1, 1}, 8080}, destfile)
    |> Map.put(:swarm, swarm)
  end

  def stub_tracker_response, do: %{interval: 9_000, peers: []}

  def stub_tracker(_, _, _, _, _, _, _, _, _, _) do
    {:ok, stub_tracker_response()}
  end
  #
  # test "is not done when we don't have all the pieces", %{destfile: file} do
  #   peer = peer()
  #   session = new(file)
  #
  #   {session, _msgs} =
  #     Download.handle_message(session, peer.peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})
  #
  #   refute Download.done?(session)
  # end
  #
  # test "is done when we have all the pieces", %{destfile: file} do
  #   peer = peer()
  #   session = new(file)
  #
  #   {session, _msgs} =
  #     Download.handle_message(session, peer.peer_id, {:piece, %{index: 0, offset: 0, data: "tin"}})
  #
  #   {session, _msgs} =
  #     Download.handle_message(session, peer.peer_id, {:piece, %{index: 1, offset: 0, data: "y\n"}})
  #
  #   assert Download.done?(session)
  # end
  #
  # test "sends torrent's download progress in announce", %{destfile: file} do
  #   peer = peer()
  #
  #   {session, _msg} =
  #     new(file)
  #     |> Download.handle_message(
  #       peer.peer_id,
  #       {:piece, %{index: 0, offset: 0, data: "tin"}}
  #     )
  #
  #   assert Pieces.bytes_completed(session.pieces) == 3
  #
  #   params = Download.announce_params(session, :interval)
  #   assert [_, _, _, _, _, _, 3, 2, opts] = params
  #   assert Keyword.get(opts, :event) == :interval
  # end
  #
  # test "includes peer_id in successive announcements", %{destfile: file} do
  #   session = new(file)
  #
  #   {session, _messages} =
  #     Download.handle_tracker_response(session, %{
  #       trackerid: "this is my tracker id",
  #       interval: 90_000,
  #       peers: []
  #     })
  #
  #   response = Download.announce_params(session, :interval)
  #
  #   assert [_, _, _, _, _, _, _, _, opts] = response
  #   assert Keyword.get(opts, :event) == :interval
  #   assert Keyword.get(opts, :trackerid) == "this is my tracker id"
  # end
  #
  # test "sends HAVE messages once it finishes a piece", %{destfile: file} do
  #   peer = peer()
  #
  #   {_session, msgs} =
  #     new(file)
  #     |> Download.handle_message(
  #       peer.peer_id,
  #       {:piece, %{index: 0, offset: 0, data: "tin"}}
  #     )
  #
  #   assert Enum.member?(msgs, {:broadcast, {:have, 0}})
  # end
  #
  # test "returns :announce message on start", %{destfile: file} do
  #   {_dl, messages} = Download.start(new(file))
  #   assert [{:announce, _params}] = messages
  # end

  # test "saves peers that result from announce", %{destfile: file} do
  #   tracker_response = %{
  #     interval: 9_000,
  #     peers: [
  #       %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
  #     ]
  #   }
  #
  #   {session, _messages} =
  #     Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
  #     |> Download.handle_tracker_response(tracker_response)
  #
  #   peer = Swarm.peer_for_address(session.swarm, {{127, 0, 0, 1}, 8001})
  #
  #   assert peer.peer_id == "remote peer id~~~~~~"
  # end
  #
  # test "does not save the same peer ID twice", %{destfile: file} do
  #   tracker_response = %{
  #     interval: 9_000,
  #     peers: [
  #       %{ip: {127, 0, 0, 1}, port: 8001, peer_id: "remote peer id~~~~~~"}
  #     ]
  #   }
  #
  #   {session, _messages} =
  #     Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
  #     |> Download.handle_tracker_response(tracker_response)
  #
  #   {session, _messages} = Download.handle_tracker_response(session, tracker_response)
  #
  #   assert Enum.count(Swarm.peers(session.swarm)) == 1
  # end
  #
  # test "compact peer response", %{destfile: file} do
  #   tracker_response = %{
  #     interval: 9_000,
  #     peers: [
  #       %{ip: {127, 0, 0, 1}, port: 8001}
  #     ]
  #   }
  #
  #   {session, _messages} =
  #     Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
  #     |> Download.handle_tracker_response(tracker_response)
  #
  #   assert Enum.count(Swarm.peers(session.swarm)) == 1
  # end
  #
  # test "rejects the same ip", %{destfile: file} do
  #   tracker_response = %{
  #     interval: 9_000,
  #     peers: [
  #       %{ip: {127, 0, 0, 1}, port: 8001}
  #     ]
  #   }
  #
  #   {session, _messages} =
  #     Download.new(@torrent, {{192, 168, 1, 1}, 8080}, file)
  #     |> Download.handle_tracker_response(tracker_response)
  #
  #   assert Enum.count(Swarm.peers(session.swarm)) == 1
  # end
end
