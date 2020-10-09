defmodule Effusion.CQRS.ProcessManagers.DownloadTorrentTest do
  use ExUnit.Case
  alias Effusion.CQRS.Events.PeerConnected
  alias Effusion.CQRS.Commands.SendBitfield
  alias Effusion.CQRS.ProcessManagers.DownloadTorrent
  doctest Effusion.CQRS.ProcessManagers.DownloadTorrent

  test "handles PeerConnected" do
    peer_uuid = UUID.uuid4()
    command =
      DownloadTorrent.handle(
        %DownloadTorrent{pieces: IntSet.new()},
        %PeerConnected{peer_uuid: peer_uuid, info_hash: TestHelper.mint_info_hash(), initiated_by: :us}
      )

    assert command.peer_uuid == peer_uuid
    assert command.bitfield == <<>>
  end
end
