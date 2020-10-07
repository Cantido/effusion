defmodule Effusion.CQRS.EventHandlers.TrackerAnnouncer do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.CQRS.Events.{
    DownloadStopped
  }
  require Logger

  def handle(
    %DownloadStopped{
      info_hash: info_hash,
      announce: announce,
      bytes_uploaded: bytes_uploaded,
      bytes_downloaded: bytes_downloaded,
      bytes_left: bytes_left,
      tracker_event: tracker_event
    },
    _metadata
  ) do
    Logger.debug("***** download stopped, disconnecting peers and contacting tracker")
    Effusion.PWP.ConnectionRegistry.disconnect_all(Effusion.Hash.decode(info_hash))

    thp_client  = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)

    opts = [event: tracker_event, numwant: 0]

    {:ok, _res} = thp_client.announce(
      announce,
      local_host,
      local_port,
      peer_id,
      info_hash,
      bytes_uploaded,
      bytes_downloaded,
      bytes_left,
      opts
    )

    # We don't care about peers here since we've stopped

    :ok
  end

end
