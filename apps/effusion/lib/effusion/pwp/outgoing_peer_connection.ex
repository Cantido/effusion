defmodule Effusion.PWP.ProcessManagers.OutgoingPeerConnection do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  require Logger

  @derive Jason.Encoder
  defstruct []

  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.PWP.Commands.Connection.AddConnectedPeer
  alias Effusion.PWP.Handshake.Commands.{SendHandshake, TimeoutHandshake}
  alias Effusion.PWP.Events.Handshake.{
    FailedHandshake,
    SuccessfulHandshake
  }
  alias Effusion.PWP.Events.Connection.{
    AttemptingToConnect,
    ConnectionAttemptFailed,
    PeerConnectionOpened,
    PeerDisconnected
  }

  def interested?(%AttemptingToConnect{peer_uuid: peer_uuid}) do
    {:start!, peer_uuid}
  end

  def interested?(%PeerConnectionOpened{peer_uuid: peer_uuid}) do
    {:continue!, peer_uuid}
  end

  def interested?(%SuccessfulHandshake{peer_uuid: peer_uuid, initiated_by: "us"}) do
    {:continue!, peer_uuid}
  end

  def interested?(%FailedHandshake{peer_uuid: peer_uuid}) do
    {:stop, peer_uuid}
  end

  def interested?(%ConnectionAttemptFailed{peer_uuid: peer_uuid}) do
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
    timeout = Application.fetch_env!(:effusion, :handshake_timeout)
    unless timeout == :infinity do
      Task.start(fn ->
        Process.sleep(timeout)
        CQRS.dispatch(%TimeoutHandshake{peer_uuid: peer_uuid})
      end)
    end
    %SendHandshake{
      peer_uuid: peer_uuid,
      our_peer_id: Application.fetch_env!(:effusion, :peer_id) |> Effusion.Hash.encode(),
      our_extensions: Application.fetch_env!(:effusion, :enabled_extensions),
      initiated_by: "us"
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
