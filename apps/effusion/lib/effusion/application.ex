defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    port = Application.get_env(:effusion, :port)

    children = [
      {Finch, name: EffusionFinch},
      {DynamicSupervisor, strategy: :one_for_one, name: Effusion.PeerSupervisor},
      {DynamicSupervisor, strategy: :one_for_one, name: Effusion.ConnectionSupervisor},
      {Registry, keys: :unique, name: PeerRegistry},
      {Registry, keys: :unique, name: DownloadRegistry},
      {Registry, keys: :unique, name: AvailabilityRegistry},
      {Registry, keys: :unique, name: ConnectionRegistry},
      :ranch.child_spec(:pwp, :ranch_tcp, [port: port], Effusion.TCPWorker, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
