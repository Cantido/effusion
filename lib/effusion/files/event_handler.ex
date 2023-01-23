defmodule Effusion.Files.EventHandler do
  use Solvent.Subscriber,
    types: ["io.github.cantido.effusion.blocks.received"]

  alias Effusion.Torrents

  def handle_event("io.github.cantido.effusion.blocks.received", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    
    if Torrents.downloading?(event.subject) do
      Effusion.Files.write_block(
        event.subject,
        event.data.index,
        event.data.offset,
        event.data.data
      )
    end
  end
end
