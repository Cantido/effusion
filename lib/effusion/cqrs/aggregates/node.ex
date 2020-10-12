defmodule Effusion.CQRS.Aggregates.Node do
  alias Effusion.CQRS.Commands.{
    AddDHTNode,
    StartDHTNode,
    GetPeers
  }
  alias Effusion.CQRS.Events.{
    DHTNodeStarted,
    DHTNodeAdded,
    GettingPeers
  }
  alias Effusion.DHT

  defstruct [
    :primary_node_id,
    :node_id,
    :routing_table,
    :host,
    :port
  ]

  def execute(
    %__MODULE__{node_id: nil},
    %StartDHTNode{node_id: node_id}
  ) do
    %DHTNodeStarted{node_id: node_id}
  end

  def execute(
    %__MODULE__{node_id: nil},
    %AddDHTNode{primary_node_id: primary_node_id, node_id: node_id, host: host, port: port}
  ) do
    %DHTNodeAdded{primary_node_id: primary_node_id, node_id: node_id, host: host, port: port}
  end

  def execute(%__MODULE__{}, %StartDHTNode{}) do
    {:error, :node_already_exists}
  end

  def execute(
    %__MODULE__{
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port
    },
    %GetPeers{info_hash: info_hash}
  ) do
    %GettingPeers{
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port,
      info_hash: info_hash,
      transaction_id: DHT.transaction_id()
    }
  end

  def apply(
    %__MODULE__{node_id: nil} = node,
    %DHTNodeStarted{node_id: node_id}
  ) do
    %__MODULE__{node |
      primary_node_id: node_id,
      node_id: node_id
    }
  end

  def apply(
    %__MODULE__{node_id: nil} = node,
    %DHTNodeAdded{
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port
    }
  ) do
    %__MODULE__{node |
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port
    }
  end

  def apply(
    %__MODULE__{} = node,
    %GettingPeers{}
  ) do
    node
  end
end
