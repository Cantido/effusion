defmodule Effusion.PeerSupervisor do
  use Supervisor

  @name Effusion.PeerSupervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: @name)
  end

  def new_peer(id) do
    Supervisor.start_child(@name, [id])
  end

  def init(:ok) do
    Supervisor.init([Effusion.Peer], strategy: :simple_one_for_one)
  end
end
