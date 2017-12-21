defmodule Effusion.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      {Task.Supervisor, name: Effusion.TaskSupervisor},
      Supervisor.child_spec({Task, fn -> Effusion.MessageServer.accept(4040) end}, restart: :permanent)
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
