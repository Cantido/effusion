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
    PeerConnectionOpened,
    SuccessfulHandshake,
    FailedHandshake,
    PeerDisconnected
  }

  def interested?(%AttemptingToConnect{peer_uuid: peer_uuid}) do
    {:start!, peer_uuid}
  end

  def interested?(%PeerConnectionOpened{peer_uuid: peer_uuid}) do
    {:continue!, peer_uuid}
  end

  def interested?(%SuccessfulHandshake{peer_uuid: peer_uuid, initiated_by: :us}) do
    {:continue!, peer_uuid}
  end

  def interested?(%FailedHandshake{peer_uuid: peer_uuid}) do
    {:stop, peer_uuid}
  end

  def interested?(%PeerDisconnected{peer_uuid: peer_uuid}) do
    {:stop, peer_uuid}
  end

  def handle(
    %__MODULE__{},
    %PeerConnectionOpened{
      peer_uuid: peer_uuid
    }
  ) do
    %SendHandshake{
      peer_uuid: peer_uuid,
      our_peer_id: Application.fetch_env!(:effusion, :peer_id),
      our_extensions: Application.fetch_env!(:effusion, :enabled_extensions),
      initiated_by: :us
    }
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
