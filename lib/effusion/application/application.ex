defmodule Effusion.Application do
  @moduledoc false
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats

  use Application

  def start(_type, _args) do
    NetStats.init()
    PeerStats.init()
    SessionStats.init()

    children = [
      Effusion.Application.BTPHandlerSupervisor,
      Effusion.Application.ConnectionSupervisor,
      Effusion.Application.VerifierWatchdogSupervisor,
      Effusion.Application.DownloadSpeedWatcherSupervisor,
      Effusion.Application.AnnouncerSupervisor,
      Effusion.BTP.Metainfo.Directory,
      Effusion.IOServer,
      Effusion.Statistics.PeerDownloadAverage,
      Effusion.Statistics.SessionDownloadAverage,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: BTPHandlerRegistry},
      {Registry, keys: :unique, name: VerifierWatchdogRegistry},
      {Registry, keys: :unique, name: AnnouncerRegistry},
      EffusionWeb.Endpoint,
      Effusion.Repo
    ]

    {:ok, _listener} =
      :ranch.start_listener(:pwp, 100, :ranch_tcp, [port: 8001], Effusion.PWP.TCP.IncomingHandler, [])

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    EffusionWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
