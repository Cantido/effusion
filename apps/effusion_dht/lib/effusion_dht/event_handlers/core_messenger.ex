defmodule Effusion.CQRS.EventHandlers.CoreMessenger do
  use Commanded.Event.Handler,
    application: Effusion.DHT.CQRS,
    name: __MODULE__

  alias Effusion.CQRS.Contexts.Peers
  alias Effusion.CQRS.Events.ReceivedPeersMatching

  def handle(
    %ReceivedPeersMatching{
      peers: peers,
      info_hash: info_hash
    },
    _metadata
  ) do
    Enum.map(peers, fn {host, port} ->
      {:ok, host} = :inet.parse_address(String.to_charlist(host))
      Peers.add(Effusion.Hash.decode(info_hash), host, port, "dht")
    end)
    :ok
  end
end
