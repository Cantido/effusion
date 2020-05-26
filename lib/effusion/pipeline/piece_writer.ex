defmodule Effusion.Pipeline.PieceWriter do
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.ProtocolHandler
  alias Effusion.Repo
  require Logger
  use GenStage

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [Effusion.Pipeline.PieceVerifier]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(piece = %Piece{}) do
    piece = Repo.preload(piece, [:torrent])
    info_hash = piece.torrent.info_hash
    index = piece.index

    Effusion.IO.write_piece(piece)
    Effusion.BTP.Pieces.mark_piece_written(info_hash, index)

    if Pieces.all_written?(info_hash) && !Torrent.finished?(info_hash) do
      Logger.debug("All pieces are written, notifying BTP handler")
      ProtocolHandler.notify_all_pieces_written(info_hash)
    end
  end
end
