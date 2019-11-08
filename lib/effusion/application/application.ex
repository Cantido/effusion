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
      Effusion.Application.DownloadServerSupervisor,
      Effusion.Application.ConnectionSupervisor,
      Effusion.BTP.Metainfo.Directory,
      Effusion.IOServer,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: SessionRegistry}
    ]

    {:ok, _listener} = :ranch.start_listener(:pwp, 100, :ranch_tcp, [port: 8001], Effusion.PWP.IncomingHandler, [])

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
