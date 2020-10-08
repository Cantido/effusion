defmodule Effusion.CQRS.ProcessManagers.OutgoingPeerConnection do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  require Logger

  defstruct []

  alias Effusion.CQRS.Commands.{
    AddConnectedPeer
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    SuccessfulHandshake,
    PeerDisconnected
  }

  def interested?(%AttemptingToConnect{internal_peer_id: internal_peer_id}) do
    {:start!, internal_peer_id}
  end

  def interested?(%SuccessfulHandshake{internal_peer_id: internal_peer_id, initiated_by: :us}) do
    {:continue!, internal_peer_id}
  end

  def interested?(%PeerDisconnected{internal_peer_id: internal_peer_id}) do
    {:stop, internal_peer_id}
  end

  def handle(
    %__MODULE__{},
    %SuccessfulHandshake{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  ) do
    %AddConnectedPeer{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  end
end
