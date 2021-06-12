defmodule Effusion.Tracker.Worker do
  alias Effusion.ActiveDownload
  alias Effusion.Swarm
  require Logger

  def announce(request) do
    worker = Application.fetch_env!(:effusion, :tracker_worker)

    worker.announce(request)
    |> case do
      {:ok, response} ->
        Enum.each(response.peers, fn response_peer ->
          address = {response_peer.ip, response_peer.port}

          Swarm.add_peer(response_peer.ip, response_peer.port)
          if response_peer[:peer_id] do
            Swarm.set_peer_id(address, response_peer[:peer_id])
          end

          ActiveDownload.add_peer(request.info_hash, address)
        end)
      err -> Logger.error("tracker returned error: #{inspect err}")
    end
  end
end
