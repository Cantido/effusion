defmodule Effusion.Torrents.EventHandler do
  use Solvent.Subscriber,
    types: [
      "io.github.cantido.effusion.peer_discovered",
      "io.github.cantido.effusion.blocks.written",
      "io.github.cantido.effusion.piece_written"
    ]

  alias Effusion.Torrents
  require Logger
  
  def handle_event("io.github.cantido.effusion.peer_discovered", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{address: address} = event.data

    if Torrents.downloading?(info_hash) do
      Logger.debug("Discovered peer at #{inspect address}, adding to torrent #{Effusion.Hash.encode(info_hash)}")

      Torrents.add_peer(info_hash, address)
    end
  end

  def handle_event("io.github.cantido.effusion.blocks.written", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{
      index: index,
      offset: offset,
      size: size
    } = event.data
    
    if Torrents.downloading?(info_hash) do
      Torrents.block_written(info_hash, index, offset, size)
    end
  end

  def handle_event("io.github.cantido.effusion.piece_written", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    meta = Torrents.get_meta(info_hash)
    %{index: index} = event.data
    
    if Torrents.downloading?(info_hash) do
      Torrents.piece_written(info_hash, index)
    end
  end
end

