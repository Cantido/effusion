defmodule Effusion.CQRS.Supervisor do
  use Supervisor

  alias Effusion.CQRS.ProcessManagers
  alias Effusion.CQRS.EventHandlers

  def start_link(_args) do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(_arg) do
    children = [
      ProcessManagers.DownloadTorrent,
      ProcessManagers.IncomingPeerConnection,
      ProcessManagers.OutgoingPeerConnection,
      ProcessManagers.DHTProtocol,
      EventHandlers.FileWriter,
      EventHandlers.TrackerAnnouncer,
      EventHandlers.PeerMessenger,
      EventHandlers.NetStatsUpdater,
      EventHandlers.PeerStatsUpdater,
      EventHandlers.SessionStatsUpdater,
      EventHandlers.NodeMessenger,
      Effusion.CQRS.Projectors.Node
    ]

    opts = [strategy: :one_for_one, name: Effusion.CQRS.Supervisor]
    Supervisor.init(children, opts)
  end
end
