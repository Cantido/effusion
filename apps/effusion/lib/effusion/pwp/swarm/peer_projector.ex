defmodule Effusion.PWP.Swarm.PeerProjector do
  use Commanded.Projections.Ecto,
    application: Effusion.Commanded,
    repo: Effusion.Repo,
    name: __MODULE__,
    consistency: :strong

  alias Effusion.PWP.Swarm.Events.PeerAddressAdded
  alias Effusion.PWP.Swarm.PeerProjection

  require Logger

  project %PeerAddressAdded{
    peer_uuid: uuid,
    expected_info_hash: info_hash,
    host: host,
    port: port
  }, _metadata, fn multi ->
    Ecto.Multi.insert(multi, :peer, %PeerProjection{
      id: uuid,
      info_hash: info_hash,
      host: host,
      port: port
    })
  end
end
