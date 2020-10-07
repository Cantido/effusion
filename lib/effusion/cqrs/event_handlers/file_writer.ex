defmodule Effusion.CQRS.EventHandlers.FileWriter do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: "Effusion.CQRS.EventHandlers.FileWriter"

  alias Effusion.CQRS.Events.{
    BlockStored,
    PieceHashSucceeded,
    AllPiecesVerified
  }
  alias Effusion.BTP.Piece
  alias Effusion.IO
  alias Effusion.Repo
  import Ecto.Query
  require Logger


end
