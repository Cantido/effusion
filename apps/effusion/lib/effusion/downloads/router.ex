defmodule Effusion.Downloads.Router do
  use Commanded.Commands.Router
  alias Effusion.Downloads.Torrent
  alias Effusion.Downloads.Commands

  dispatch(
    [
      Commands.AddTorrent,
      Commands.StartDownload,
      Commands.StopDownload,
      Commands.StoreBlock,
      Commands.HandleCompletedDownload
    ],
    to: Torrent,
    identity: :info_hash
  )
end
