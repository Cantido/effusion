defmodule Effusion.BTP.VerifiedPieceAnnouncer do
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.BTP.Piece
  alias Effusion.Repo
  use GenStage

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [Effusion.BTP.VerifiedPieceProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event(piece) do
    ConnectionRegistry.btp_broadcast(piece.info_hash, {:have, piece.index})
    Repo.get(Piece, piece.id)
    |> Ecto.Changeset.change(announced: true)
    |> Repo.update!()
  end
end
