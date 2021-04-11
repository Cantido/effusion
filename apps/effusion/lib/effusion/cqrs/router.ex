defmodule Effusion.CQRS.Router do
  use Commanded.Commands.Router
  alias Effusion.CQRS.Aggregates.Torrent
  alias Effusion.CQRS.Commands


  dispatch [
    Commands.AddTorrent,
    Commands.StartDownload,
    Commands.StopDownload,
    Commands.StoreBlock,
    Commands.HandleCompletedDownload
  ], to: Torrent, identity: :info_hash
end
