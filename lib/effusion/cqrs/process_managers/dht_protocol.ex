defmodule Effusion.CQRS.ProcessManagers.DHTProtocol do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.CQRS.Events.{
    DHTNodeStarted
  }

  defstruct [
    node_id: nil
  ]

  def interested?(%DHTNodeStarted{node_id: node_id}) do
    {:start!, node_id}
  end

  def apply(
    %__MODULE__{node_id: nil} = dht,
    %DHTNodeStarted{node_id: node_id}
  ) do
    %__MODULE__{dht |
      node_id: node_id
    }
  end
end
