defmodule Effusion.Application do
  @moduledoc false
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats
  alias Effusion.Statistics.TelemetryHandler

  use Application

  def start(_type, _args) do
    NetStats.init()
    PeerStats.init()
    SessionStats.init()
    TelemetryHandler.init()

    children = [
      Effusion.Repo,
      Effusion.Statistics.Supervisor,
      # {Effusion.BlockingQueue, name: PieceQueue},
      # {Effusion.BlockingQueueProducer, name: PieceProducer, queue: PieceQueue, dispatcher: GenStage.BroadcastDispatcher},
      Effusion.BTP.VerifiedPieceProducer,
      Effusion.BTP.VerifiedPieceAnnouncer,
      Effusion.IO.PieceWriter,
      Effusion.Application.ConnectionSupervisor,
      Effusion.Application.DownloadsSupervisor,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: BTPHandlerRegistry},
      {Registry, keys: :unique, name: VerifierWatchdogRegistry},
      {Registry, keys: :unique, name: AnnouncerRegistry},
      # Effusion.BTP.Session,
      EffusionWeb.Endpoint
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
