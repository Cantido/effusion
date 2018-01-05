defmodule Effusion.PWP.Connection.Supervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def start_child(client) do
    Supervisor.start_child(__MODULE__, [client])
  end

  def init(:ok) do
    Supervisor.init(
    [Effusion.PWP.Connection],
    strategy: :simple_one_for_one)
  end
end
