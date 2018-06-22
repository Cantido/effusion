defmodule Effusion.BlockStage do
  require Logger
  use GenStage
  alias Effusion.BTP.Torrent

  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def put_event(event) do
    GenStage.call(__MODULE__, {:put_event, event})
  end

  def init(:ok) do
    {:producer, MapSet.new()}
  end

  def handle_call({:put_event, {info_hash, info, file, block}}, _from, state) do
    Logger.debug("Handling a block in BlockStage")
    torrent = Map.get(state, info_hash, Torrent.new(info))
    |> Torrent.add_block(block)

    verified = Torrent.verified(torrent)
    torrent = Enum.reduce(verified, torrent, fn(p, t) -> Torrent.mark_piece_written(t, p) end)
    state = Map.put(state, info_hash, torrent)

    events = Enum.map(verified, fn p -> {info, file, p} end)

    {:reply, :ok, events, state}
  end

  def handle_demand(_, state)  do
    {:noreply, [], state}
  end
end
