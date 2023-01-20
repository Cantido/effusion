defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    Solvent.subscribe(Effusion.Connections.EventHandler)
    Solvent.subscribe(Effusion.Files.EventHandler)
    Solvent.subscribe(Effusion.Swarm.EventHandler)
    Solvent.subscribe(Effusion.Torrents.EventHandler)
    Solvent.subscribe(Effusion.Tracker.EventHandler)

    port = Application.get_env(:effusion, :port)

    children = [
      {Finch, name: EffusionFinch},
      {DynamicSupervisor, strategy: :one_for_one, name: Effusion.PeerSupervisor},
      {DynamicSupervisor, strategy: :one_for_one, name: Effusion.ConnectionSupervisor},
      {DynamicSupervisor, strategy: :one_for_one, name: Effusion.TorrentSupervisor},
      {Registry, keys: :unique, name: PeerRegistry},
      {Registry, keys: :unique, name: TorrentRegistry},
      {Registry, keys: :unique, name: ConnectionRegistry},
      {Effusion.Swarm, [name: Effusion.Swarm]},
      :ranch.child_spec(:pwp, :ranch_tcp, [port: port], Effusion.Connections, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
