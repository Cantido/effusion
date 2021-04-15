defmodule Effusion.DHT.Application do
  @moduledoc false

  use Application


  def start(_type, _args) do
    dht_port = Application.fetch_env!(:effusion_dht, :port)

    children = [
      Effusion.DHT.CQRS,
      {Effusion.DHT.Server, port: dht_port}
    ]

    opts = [strategy: :one_for_one, name: Effusion.DHT.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
