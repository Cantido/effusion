defmodule Effusion.Peer do
  @enforce_keys [
    :host,
    :port
  ]
  defstruct [
    id: nil,
    host: nil,
    port: nil,
    failcount: 0
  ]

  def address(peer) do
    {peer.host, peer.port}
  end

  def peer_id(peer) do
    peer.id
  end

  def set_peer_id(peer, peer_id) do
    %__MODULE__{peer | id: peer_id}
  end

  def increment_failcount(peer) do
    %__MODULE__{peer | failcount: peer.failcount + 1}
  end

  def decrement_failcount(peer) do
    %__MODULE__{peer | failcount: peer.failcount - 1}
  end
end
