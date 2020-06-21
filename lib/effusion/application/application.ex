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

    port = Application.get_env(:effusion, :port)

    children = [
      Effusion.Repo,
      Effusion.Statistics.Supervisor,
      {Queutils.BlockingProducer, name: MessageProducer, max_length: 10_000},
      Effusion.Pipeline.MessageConsumer,
      Effusion.Pipeline.PieceVerifier,
      Effusion.Pipeline.VerifiedPieceAnnouncer,
      Effusion.Pipeline.PieceWriter,
      Effusion.Application.ConnectionSupervisor,
      Effusion.Application.DownloadsSupervisor,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: BTPHandlerRegistry},
      {Registry, keys: :unique, name: FinishedTorrentWatchdogRegistry},
      {Registry, keys: :unique, name: AnnouncerRegistry},
      # Effusion.BTP.Session,
      {Effusion.DHT.Server, port: port},
      EffusionWeb.Endpoint
    ]

    {:ok, _listener} =
      :ranch.start_listener(:pwp, 100, :ranch_tcp, [port: port], Effusion.PWP.TCP.Connection, [])

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
