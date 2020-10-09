defmodule Effusion.CQRS.ProcessManagers.IncomingPeerConnection do
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
    PeerSentHandshake,
    SuccessfulHandshake,
    PeerDisconnected
  }

  def interested?(%PeerSentHandshake{peer_uuid: peer_uuid, initiated_by: :them}) do
    {:start!, peer_uuid}
  end

  def interested?(%SuccessfulHandshake{peer_uuid: peer_uuid, initiated_by: :them}) do
    {:continue!, peer_uuid}
  end

  def interested?(%PeerDisconnected{peer_uuid: peer_uuid}) do
    {:stop, peer_uuid}
  end

  def handle(
    %__MODULE__{},
    %PeerSentHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      initiated_by: :them
    }
  ) do
    Logger.debug("****** Peer sent a handshake, dispatching send handshake command")
    %SendHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: Application.fetch_env!(:effusion, :peer_id),
      our_extensions: Application.fetch_env!(:effusion, :enabled_extensions),
      initiated_by: :them
    }
  end

  def handle(
    %__MODULE__{},
    %SuccessfulHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      initiated_by: initiated_by
    }
  ) do
    Logger.debug("****** Handshake successful")
    %AddConnectedPeer{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      initiated_by: initiated_by
    }
  end
end
