defmodule Effusion.CQRS.EventHandlers.TrackerAnnouncer do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__,
    consistency: :strong

  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Events.{
    DownloadStarted,
    DownloadStopped
  }
  alias Effusion.CQRS.Commands.AddPeer
  require Logger

  def handle(
    %DownloadStarted{
      info_hash: info_hash,
      announce: announce_uri,
      bytes_left: bytes_left
    },
    _metadata
  ) do
    Logger.debug("****** Download starting, notifying tracker")

    thp_client  = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)
    tracker_numwant = Application.fetch_env!(:effusion, :max_peers)

    opts = [event: "started", numwant: tracker_numwant]

    {:ok, res} = thp_client.announce(
      announce_uri,
      local_host,
      local_port,
      peer_id,
      Effusion.Hash.decode(info_hash),
      0,
      0,
      bytes_left,
      opts
    )

    Enum.map(res.peers, fn peer ->
      host = to_string(:inet.ntoa(peer.ip))
      %AddPeer{
        peer_uuid: UUID.uuid4(),
        expected_info_hash: info_hash,
        host: host,
        port: peer.port,
        expected_peer_id: Map.get(peer, :peer_id, nil),
        from: :tracker
      }
    end)
    |> Enum.filter(fn peer ->
      peer.port > 0
    end)
    |> Enum.each(&CQRS.dispatch/1)
  end

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

    thp_client  = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)

    opts = [event: tracker_event, numwant: 0]

    {:ok, _res} = thp_client.announce(
      announce,
      local_host,
      local_port,
      peer_id,
      Effusion.Hash.decode(info_hash),
      bytes_uploaded,
      bytes_downloaded,
      bytes_left,
      opts
    )

    Logger.debug("***** stop announce done")

    # We don't care about peers here since we've stopped

    :ok
  end

end
