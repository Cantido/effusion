defmodule Effusion.PWP.Supervisor do
  use Supervisor

  def start_link(_) do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      Effusion.PWP.Connection.Supervisor,
      Effusion.PWP.Server
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
