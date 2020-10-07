defmodule Effusion.CQRS.ProcessManagers.PeerConnection do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  defstruct []

  alias Effusion.CQRS.Commands.{
    AddConnectedPeer
  }
  alias Effusion.CQRS.Events.{
    SuccessfulHandshake,
    PeerDisconnected
  }

  def interested?(%SuccessfulHandshake{info_hash: info_hash}) do
    {:start!, info_hash}
  end

  def interested?(%PeerDisconnected{info_hash: info_hash}) do
    {:stop, info_hash}
  end

  def handle(
    %__MODULE__{},
    %SuccessfulHandshake{info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  ) do
    %AddConnectedPeer{info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  end
end
