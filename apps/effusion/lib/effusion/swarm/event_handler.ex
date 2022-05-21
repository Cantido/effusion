defmodule Effusion.Swarm.EventHandler do
  use Solvent.Subscriber,
    match_type: [
      "io.github.cantido.effusion.peer_discovered"
    ]

  alias Effusion.Swarm

  def handle_event("io.github.cantido.effusion.peer_discovered", event_id) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    %{
      address: {host, port},
      peer_id: peer_id
    } = event.data

    Swarm.add_peer(host, port)

    unless is_nil(peer_id) do
      Swarm.set_peer_id({host, port}, peer_id)
    end
  end
end
