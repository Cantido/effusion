defmodule Effusion.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Effusion.SessionSupervisor
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
