defmodule Effusion.PWP.Supervisor do
  use Supervisor

  def start_link(_) do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      Effusion.PWP.Connection.Supervisor,
      {Task.Supervisor, name: Effusion.TaskSupervisor},
      Supervisor.child_spec({Task, fn -> Effusion.PWP.Server.listen(4040) end}, restart: :permanent)
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
