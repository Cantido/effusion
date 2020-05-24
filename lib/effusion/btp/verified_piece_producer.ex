defmodule Effusion.BTP.VerifiedPieceProducer do
  alias Effusion.BTP.Pieces
  alias Effusion.Repo
  use GenStage
  import Ecto.Query
  require Logger

  @poll_interval 250

  def start_link(opts) do
    GenStage.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    Process.send_after(self(), :poll, @poll_interval)
    {:producer, 0, dispatcher: GenStage.BroadcastDispatcher}
  end

  @impl true
  def handle_info(:poll, demand) do
    events = get_events(demand)

    remaining_demand = demand - Enum.count(events)

    Process.send_after(self(), :poll, @poll_interval)
    {:noreply, events, remaining_demand}
  end

  @impl true
  def handle_demand(demand, past_demand) do
    total_demand = demand + past_demand
    events = get_events(demand)
    remaining_demand = total_demand - Enum.count(events)
    {:noreply, events, remaining_demand}
  end

  defp get_events(count) do
    Repo.transaction(fn ->
      Repo.stream(
        from torrent in Effusion.BTP.Torrent,
        select: torrent.info_hash
      )
      |> Stream.flat_map(&Pieces.verified/1)
      |> Stream.take(count)
      |> Enum.to_list()
    end)
    |> case do
      {:ok, result} -> result
      _ -> []
    end
  end
end
