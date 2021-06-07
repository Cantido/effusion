defmodule Effusion.THP.Worker do
  def start(announce_uri, info_hash, bytes_left) do
    thp_client = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    local_host = Application.fetch_env!(:effusion, :host)
    local_port = Application.fetch_env!(:effusion, :port)
    tracker_numwant = Application.fetch_env!(:effusion, :max_peers)

    opts = [event: "started", numwant: tracker_numwant]

    {:ok, res} =
      thp_client.announce(
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

    res.peers
    |> Enum.filter(fn peer ->
      peer.port > 0
    end)
    |> Enum.each(fn peer ->
      Effusion.PWP.add(
        UUID.uuid4(),
        Effusion.Hash.decode(info_hash),
        Map.get(peer, :peer_id, nil),
        peer.ip,
        peer.port,
        "tracker"
      )
    end)
  end

  def stop(
    info_hash,
    announce,
    bytes_uploaded,
    bytes_downloaded,
    bytes_left,
    tracker_event
  ) do
    thp_client = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)

    opts = [event: tracker_event, numwant: 0]

    {:ok, _res} =
      thp_client.announce(
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

    # We don't care about peers here since we've stopped

    :ok
  end
end
