defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    :ok = Honeydew.start_queue(:tracker)
    :ok = Honeydew.start_workers(:tracker, Effusion.Tracker.Worker)

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
