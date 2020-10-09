defmodule Effusion.CQRS.ProcessManagers.OutgoingPeerConnection do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  require Logger

  defstruct []

  alias Effusion.CQRS.Commands.{
    AddConnectedPeer,
    SendHandshake
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    SendingHandshake,
    SuccessfulHandshake,
    PeerConnected
  }

  def interested?(%AttemptingToConnect{peer_uuid: peer_uuid}) do
    {:start!, peer_uuid}
  end

  def interested?(%SuccessfulHandshake{peer_uuid: peer_uuid, initiated_by: :us}) do
    {:continue!, peer_uuid}
  end

  def interested?(%PeerConnected{peer_uuid: peer_uuid}) do
    {:stop, peer_uuid}
  end

  def handle(
    %__MODULE__{},
    %SuccessfulHandshake{
      peer_uuid: peer_uuid,
      initiated_by: initiated_by
    }
  ) do
    %AddConnectedPeer{
      peer_uuid: peer_uuid,
      initiated_by: initiated_by
    }
  end
end
