defmodule Effusion.CQRS.ProcessManagers.IncomingPeerConnection do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  require Logger

  @derive Jason.Encoder
  defstruct []

  alias Effusion.CQRS.Commands.{
    AddConnectedPeer,
    SendHandshake,
    RemoveConnectedPeer
  }
  alias Effusion.CQRS.Events.{
    PeerSentHandshake,
    SuccessfulHandshake,
    FailedHandshake,
    PeerDisconnected
  }

  def interested?(%PeerSentHandshake{peer_uuid: peer_uuid, initiated_by: "them"}) do
    {:start!, peer_uuid}
  end

  def interested?(%SuccessfulHandshake{peer_uuid: peer_uuid, initiated_by: "them"}) do
    {:continue!, peer_uuid}
  end

  def interested?(%FailedHandshake{peer_uuid: peer_uuid, initiated_by: "them"}) do
    {:continue!, peer_uuid}
  end

  def interested?(%PeerDisconnected{peer_uuid: peer_uuid, initiated_by: "them"}) do
    {:stop, peer_uuid}
  end

  def handle(
    %__MODULE__{},
    %PeerSentHandshake{
      peer_uuid: peer_uuid,
      initiated_by: "them"
    }
  ) do
    %SendHandshake{
      peer_uuid: peer_uuid,
      our_peer_id: Application.fetch_env!(:effusion, :peer_id) |> Effusion.Hash.encode(),
      our_extensions: Application.fetch_env!(:effusion, :enabled_extensions),
      initiated_by: "them"
    }
  end

  def handle(
    %__MODULE__{},
    %FailedHandshake{
      peer_uuid: peer_uuid,
      failure_reason: failure_reason
    }
  ) do
    %RemoveConnectedPeer{
      peer_uuid: peer_uuid,
      reason: "failed handshake; reason: #{failure_reason}"
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
