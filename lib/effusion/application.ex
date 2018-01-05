defmodule Effusion.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Effusion.PeerSupervisor,
      Effusion.PeerConnectionSupervisor,
      {Task.Supervisor, name: Effusion.TaskSupervisor},
      {Registry, keys: :unique, name: Effusion.PeerRegistry},
      {Registry, keys: :unique, name: Effusion.TorrentRegistry},
      Supervisor.child_spec({Task, fn -> Effusion.MessageServer.listen(4040) end}, restart: :permanent)
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
