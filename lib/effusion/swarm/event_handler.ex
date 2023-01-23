defmodule Effusion.Swarm.EventHandler do
  use Solvent.Subscriber,
    types: [
      "io.github.cantido.effusion.peer_discovered"
    ]

  alias Effusion.Swarm

  def handle_event("io.github.cantido.effusion.peer_discovered", event_id, _) do
    {:ok, event} = Solvent.EventStore.fetch(event_id)
    {host, port} = Map.fetch!(event.data, :address)

    Swarm.add_peer(host, port)

    unless Map.has_key?(event.data, :address) and not is_nil(event.data[:address]) do
      Swarm.set_peer_id({host, port}, event.data[:address])
    end
  end
end
