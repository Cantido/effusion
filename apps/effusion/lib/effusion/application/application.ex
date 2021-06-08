defmodule Effusion.Application do
  @moduledoc false
  use Application

  def start(_type, _args) do
    port = Application.get_env(:effusion, :port)

    children = [
      :ranch.child_spec(:pwp, 100, :ranch_tcp, [port: port], Effusion.PWP.TCP.Worker, [])
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
