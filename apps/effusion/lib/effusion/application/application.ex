defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Effusion.Statistics.init()

    port = Application.get_env(:effusion, :port)

    children = [
      Effusion.Repo,
      Effusion.Commanded,
      Effusion.Statistics.PeerDownloadAverage,
      Effusion.Statistics.SessionDownloadAverage,
      Effusion.Statistics.SessionUploadAverage,
      Effusion.Statistics.EventHandlers.NetStatsUpdater,
      Effusion.Statistics.EventHandlers.PeerStatsUpdater,
      Effusion.Statistics.EventHandlers.SessionStatsUpdater,
      Effusion.Downloads.ProcessManagers.DownloadTorrent,
      Effusion.Downloads.EventHandlers.FileWriter,
      Effusion.THP.EventHandlers.TrackerAnnouncer,
      Effusion.PWP.ProcessManagers.IncomingPeerConnection,
      Effusion.PWP.ProcessManagers.OutgoingPeerConnection,
      Effusion.PWP.EventHandlers.PeerMessenger,
      Effusion.PWP.Swarm.PeerProjector,
      Effusion.Application.ConnectionSupervisor,
      {Registry, keys: :unique, name: ConnectionRegistry},
      :ranch.child_spec(:pwp, 100, :ranch_tcp, [port: port], Effusion.PWP.TCP.Connection, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
