defmodule Effusion.Downloads.EventHandlers.FileWriter do
  use Commanded.Event.Handler,
    application: Effusion.Commanded,
    name: __MODULE__,
    consistency: :strong

  alias Effusion.Downloads.Events.PieceHashSucceeded
  alias Effusion.Range
  require Logger

  def handle(
        %PieceHashSucceeded{info_hash: info_hash, index: index, data: data, info: info},
        _metadata
      ) do
    Logger.debug("**** CQRS is writing piece #{index} of #{info_hash}")
    data = Base.decode64!(data)

    Effusion.IO.write_piece(data, info_hash, index, info)

    :ok
  end
end
