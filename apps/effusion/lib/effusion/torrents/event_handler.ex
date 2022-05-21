defmodule Effusion.Torrents.EventHandler do use Solvent.Subscriber,
    match_type: [
      "io.github.cantido.effusion.peer_discovered",
      "io.github.cantido.effusion.blocks.written",
      "io.github.cantido.effusion.piece_written"
    ]

  alias Effusion.Torrents
  require Logger
  
  def handle_event("io.github.cantido.effusion.peer_discovered", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{address: address} = event.data

    if Torrents.downloading?(info_hash) do
      Logger.debug("Discovered peer at #{inspect address}, adding to torrent #{Effusion.Hash.encode(info_hash)}")

      Torrents.add_peer(info_hash, address)
    end
  end

  def handle_event("io.github.cantido.effusion.blocks.written", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    %{
      index: index,
      offset: offset,
      size: size
    } = event.data
    
    if Torrents.downloading?(info_hash) do
      Logger.debug("Wrote block at index #{index} offset #{offset} size #{size} for torrent #{Effusion.Hash.encode(info_hash)}, cancelling requests")

      Torrents.block_written(info_hash, index, offset)

      Torrents.pop_requests(info_hash, index, offset, size)
      |> Enum.each(fn address ->
        Solvent.publish(
          "io.github.cantido.effusion.request_cancelled",
          subject: info_hash,
          data: %{
            address: address,
            index: index,
            offset: offset,
            size: size
          }
        )
      end)
    end
  end

  def handle_event("io.github.cantido.effusion.piece_written", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject
    meta = Torrents.get_meta(info_hash)
    %{index: index} = event.data
    
    if Torrents.downloading?(info_hash) do
      Logger.debug("Write piece #{index} of torrent #{Effusion.Hash.encode(info_hash)}, verifying piece")

      {:ok, data} = Effusion.Files.IO.read_block(index, 0, meta.info.piece_length, meta.info)

      expected_hash = Enum.at(meta.info.pieces, index)
      actual_hash = Effusion.Hash.calc(data)

      if expected_hash == actual_hash do
        Torrents.piece_verified(info_hash, index)

        Solvent.publish(
          "io.github.cantido.effusion.piece_verified",
          subject: info_hash,
          data: %{index: index}
        )

        {bytes_completed, finished_length} = Torrents.get_progress(info_hash)

        if bytes_completed == finished_length do
          Solvent.publish(
            "io.github.cantido.effusion.torrent_completed",
            subject: info_hash
          )
        end
      else
        Torrents.piece_failed_verification(info_hash, index)

        Solvent.publish(
          "io.github.cantido.effusion.piece_failed",
          subject: info_hash,
          data: %{index: index}
        )
      end
    end
  end
end

