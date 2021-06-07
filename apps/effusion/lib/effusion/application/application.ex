defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Effusion.Statistics.init()
    :ok = Honeydew.start_queue(:announce)

    port = Application.get_env(:effusion, :port)

    children = [
      Effusion.Repo,
      Effusion.Statistics.PeerDownloadAverage,
      Effusion.Statistics.SessionDownloadAverage,
      Effusion.Statistics.SessionUploadAverage,
      Effusion.Application.ConnectionSupervisor,
      {Registry, keys: :unique, name: ConnectionRegistry},
      :ranch.child_spec(:pwp, 100, :ranch_tcp, [port: port], Effusion.PWP.TCP.Connection, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
