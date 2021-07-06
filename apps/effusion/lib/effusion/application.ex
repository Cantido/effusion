defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    :ok = start_honeydew(:tracker, Effusion.Tracker.Worker)
    :ok = start_honeydew(:file, Effusion.FileWorker)
    :ok = start_honeydew(:connection, Effusion.ConnectionWorker)

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
      :ranch.child_spec(:pwp, :ranch_tcp, [port: port], Effusion.TCPWorker, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp start_honeydew(queue, worker) do
    with :ok <- Honeydew.start_queue(queue) do
      Honeydew.start_workers(queue, worker)
    end
  end
end
