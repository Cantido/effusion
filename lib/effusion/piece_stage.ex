defmodule Effusion.PieceStage do
  use GenStage
  require Logger
  alias Effusion.BTP.Torrent
  import Effusion.Hash

  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def put_event(event) do
    GenStage.call(__MODULE__, {:put_event, event})
  end

  def init(:ok) do
    {:producer, Map.new()}
  end

  def handle_call({:put_event, {info_hash, info, file, block}}, _from, torrents)
      when is_hash(info_hash) do
    {verified, torrents} =
      Effusion.Map.get_and_update(
        torrents,
        info_hash,
        &Torrent.add_block_and_take_verified(&1, block),
        Torrent.new(info_hash)
      )

    events = Enum.map(verified, fn p -> {info, file, p} end)

    {:reply, :ok, events, torrents}
  end

  def handle_demand(_, state) do
    {:noreply, [], state}
  end
end
