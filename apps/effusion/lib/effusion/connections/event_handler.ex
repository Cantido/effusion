defmodule Effusion.Connections.EventHandler do
  use Solvent.Subscriber,
    match_type: [
      "io.github.cantido.effusion.interested",
      "io.github.cantido.effusion.piece_verified",
      "io.github.cantido.effusion.request_cancelled",
      "io.github.cantido.effusion.torrent_stopped"
    ]

  alias Effusion.Connections
  require Logger

  def handle_event("io.github.cantido.effusion.interested", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{address: address} = event.data

    Logger.debug("Interested in peer #{inspect address} for torrent #{Effusion.Hash.encode(info_hash)}")

    Connections.send(info_hash, address, :interested)
  end

  def handle_event("io.github.cantido.effusion.piece_verified", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{index: index} = event.data

    Logger.debug("Verified piece #{index} of torrent #{Effusion.Hash.encode(info_hash)}, broadcasting HAVE message")
      
    Connections.broadcast(info_hash, {:have, index})
  end

  def handle_event("io.github.cantido.effusion.request_cancelled", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{
      address: address,
      index: index,
      offset: offset,
      size: size
    } = event.data

    Connections.send(info_hash, address, {:cancel, index, offset, size})
  end

  def handle_event("io.github.cantido.effusion.torrent_stopped", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject

    Logger.debug("Torrent stopped, disconnecting all peers for torrent #{Effusion.Hash.encode(info_hash)}")

    Connections.disconnect_all(info_hash)
  end
end

