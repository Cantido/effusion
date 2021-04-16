defmodule Effusion.DHT.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Effusion.DHT.CQRS
    ]

    opts = [strategy: :one_for_one, name: Effusion.DHT.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
