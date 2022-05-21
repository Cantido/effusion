defmodule Effusion.Files.EventHandler do
  use Solvent.Subscriber,
    match_type: "io.github.cantido.effusion.blocks.received"

  alias Effusion.Torrents
  require Logger

  def handle_event("io.github.cantido.effusion.blocks.received", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)

    Logger.debug("Informing torrent of received block")
    
    unless Torrents.block_written?(event.subject, event.data.index, event.data.offset) do
      meta = Torrents.get_meta(event.subject)

      :ok = Effusion.Files.IO.write_block(
        event.data.data,
        event.data.index,
        event.data.offset,
        meta.info
      )

      Solvent.publish(
        "io.github.cantido.effusion.blocks.written",
        subject: event.subject,
        data: %{
          index: event.data.index,
          offset: event.data.offset,
          size: byte_size(event.data.data)
        }
      )
    end
  end
end
