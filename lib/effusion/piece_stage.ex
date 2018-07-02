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
    torrent = Map.get(torrents, info_hash, Torrent.new(info))
    |> Torrent.add_block(block)

    verified = Torrent.verified(torrent)

    torrent = Enum.reduce(verified, torrent, fn p, t ->
      Torrent.mark_piece_written(t, p)
    end)

    events = verified
    |> Enum.map(fn p -> {info, file, p} end)

    torrents = Map.put(torrents, info_hash, torrent)
    {:reply, :ok, events, torrents}
  end

  def handle_demand(_, state)  do
    {:noreply, [], state}
  end
end
