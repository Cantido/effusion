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

    port = Application.get_env(:effusion, :port)

    children = [
      Effusion.Repo,
      Effusion.Statistics.Supervisor,
      Effusion.CQRS.Application,
      Effusion.CQRS.Supervisor,
      Effusion.Application.ConnectionSupervisor,
      {Registry, keys: :unique, name: ConnectionRegistry},
      :ranch.child_spec(:pwp, 100, :ranch_tcp, [port: port], Effusion.PWP.TCP.Connection, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
