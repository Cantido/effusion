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
    {:consumer, 0, [subscribe_to: [PieceProducer]]}
  end

  @doc """
  Consume a GenStage event to write a piece.
  """
  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &handle_event/1)
    {:noreply, [], state}
  end

  defp handle_event({info_hash, piece}) do
    ConnectionRegistry.btp_broadcast(info_hash, {:have, piece.index})
    Repo.get(Piece, piece.id)
    |> Ecto.Changeset.change(announced: true)
    |> Repo.update!()
  end
end
