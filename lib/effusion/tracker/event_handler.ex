defmodule Effusion.Tracker.EventHandler do
  use Solvent.Subscriber,
    types: [
      "io.github.cantido.effusion.torrent.started",
      "io.github.cantido.effusion.torrent_completed",
      "io.github.cantido.effusion.torrent_stopped"
    ]
  alias Effusion.Torrents
  require Logger

  def handle_event("io.github.cantido.effusion.torrent.started", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject

    meta = Torrents.get_meta(info_hash)
    uploaded = Torrents.get_uploaded(info_hash)
    {downloaded, total_bytes} = Torrents.get_progress(info_hash)
    left = total_bytes - downloaded 
    

    request =
      %Effusion.Tracker.Request{
        url: meta.announce,
        ip: Application.fetch_env!(:effusion, :host),
        port: Application.fetch_env!(:effusion, :port),
        peer_id: Application.fetch_env!(:effusion, :peer_id),
        info_hash: info_hash,
        uploaded: uploaded,
        downloaded: downloaded,
        left: left,
        event: "started",
        numwant: Application.fetch_env!(:effusion, :max_peers)
      }

    Logger.debug("Making tracker request: #{inspect request, pretty: true}")

    announce(request)
  end

  def handle_event("io.github.cantido.effusion.torrent_completed", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject

    meta = Torrents.get_meta(info_hash)
    uploaded = Torrents.get_uploaded(info_hash)
    {downloaded, total} = Torrents.get_progress(info_hash)
    left = total - downloaded

    request =
      %Effusion.Tracker.Request{
        url: meta.announce,
        ip: Application.fetch_env!(:effusion, :host),
        port: Application.fetch_env!(:effusion, :port),
        peer_id: Application.fetch_env!(:effusion, :peer_id),
        info_hash: info_hash,
        uploaded: uploaded,
        downloaded: downloaded,
        left: left,
        event: "completed",
        numwant: 0
      }

    announce(request)
  end

  def handle_event("io.github.cantido.effusion.torrent_stopped", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    info_hash = event.subject

    request =
      %Effusion.Tracker.Request{
        url: event.data.announce,
        ip: Application.fetch_env!(:effusion, :host),
        port: Application.fetch_env!(:effusion, :port),
        peer_id: Application.fetch_env!(:effusion, :peer_id),
        info_hash: info_hash,
        uploaded: event.data.uploaded,
        downloaded: event.data.downloaded,
        left: event.data.left,
        event: "stopped",
        numwant: 0
      }

    announce(request)
  end

  defp announce(request) do
    worker = Application.fetch_env!(:effusion, :tracker_worker)

    Logger.debug("Announcing #{request.event} event to #{request.url}.")

    worker.announce(request)
    |> case do
      {:ok, response} ->
        unless request.numwant == 0 do
          Logger.debug("Received #{Enum.count(response.peers)} peers from #{request.url}.")
          Enum.each(response.peers, fn response_peer ->
            Solvent.publish(
              "io.github.cantido.effusion.peer_discovered",
              subject: request.info_hash,
              data: %{
                address: {response_peer[:ip], response_peer[:port]},
                peer_id: response_peer[:peer_id]
              }
            )
          end)
        end
      err -> Logger.error("tracker returned error: #{inspect err}")
    end
  end
end
