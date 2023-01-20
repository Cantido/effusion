defmodule Effusion.DHT.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Effusion.DHT.ServerManager, node_id: Effusion.DHT.local_node_id(), name: Effusion.DHT.ServerManager},
      {Effusion.DHT.UDPListener, port: Application.fetch_env!(:effusion_dht, :port), server: Effusion.DHT.ServerManager}
    ]

    opts = [strategy: :one_for_one, name: Effusion.DHT.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
