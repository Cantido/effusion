defmodule Effusion.DHT.Peer do
  def compact(%Effusion.PWP.Swarm.PeerProjection{} = peer) do
    {:ok, {ip0, ip1, ip2, ip3}} = :inet.parse_ipv4strict_address(String.to_charlist(peer.host))
    <<ip0, ip1, ip2, ip3>> <> <<peer.port::integer-size(16)>>
  end
end
