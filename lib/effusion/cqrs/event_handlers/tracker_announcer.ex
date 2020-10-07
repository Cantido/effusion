defmodule Effusion.CQRS.EventHandlers.TrackerAnnouncer do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: "Effusion.CQRS.EventHandlers.TrackerAnnouncer"

  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.AddPeer
  alias Effusion.CQRS.Events.DownloadStarted
  require Logger


  def handle(
    %DownloadStarted{
      info_hash: info_hash,
      announce: announce_uri,
      bytes_left: bytes_left
    },
    _metadata
  ) do
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
      info_hash,
      0,
      0,
      bytes_left,
      opts
    )

    Enum.map(res.peers, fn peer ->
      %AddPeer{
        info_hash: info_hash,
        host: peer.ip,
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)
    |> Enum.filter(fn peer ->
      peer.port > 0
    end)
    |> log()
    |> Enum.each(&CQRS.dispatch/1)

    :ok
  end

  defp log(peers) do
    Logger.debug("**** CQRS just dispatched #{Enum.count(peers)} peer(s)")
    peers
  end

end
