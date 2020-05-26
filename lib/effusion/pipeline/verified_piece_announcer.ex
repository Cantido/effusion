defmodule Effusion.Pipeline.VerifiedPieceAnnouncer do
  alias Effusion.BTP.ProtocolHandler
  alias Effusion.Pipeline.PieceVerifier
  use GenStage

  @moduledoc """
  Tells peers that we have verified a piece of a torrent.
  """

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [PieceVerifier]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(piece) do
    piece = Effusion.Repo.preload(piece, [:torrent])
    ProtocolHandler.have_piece(piece.torrent.info_hash, piece)
  end
end
