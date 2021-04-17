defmodule Effusion.DHT.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {
        Effusion.DHT.UDPListener,
          port: Application.fetch_env!(:effusion_dht, :port),
          node_id: Application.fetch_env!(:effusion_dht, :node_id) |> Base.decode64!()
      },
      Effusion.DHT.Commanded
    ]

    opts = [strategy: :one_for_one, name: Effusion.DHT.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
