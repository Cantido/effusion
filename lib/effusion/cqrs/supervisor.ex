defmodule Effusion.CQRS.Supervisor do
  use Supervisor

  alias Effusion.CQRS.ProcessManagers

  def start_link(_args) do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(_arg) do
    children = [
      ProcessManagers.DownloadTorrent,
      ProcessManagers.IncomingPeerConnection,
      ProcessManagers.OutgoingPeerConnection,
      Effusion.CQRS.EventHandlers.FileWriter,
      Effusion.CQRS.EventHandlers.TrackerAnnouncer,
      Effusion.CQRS.EventHandlers.PeerMessenger
    ]

    opts = [strategy: :one_for_one, name: Effusion.CQRS.Supervisor]
    Supervisor.init(children, opts)
  end
end
