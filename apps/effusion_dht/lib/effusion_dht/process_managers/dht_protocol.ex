defmodule Effusion.CQRS.ProcessManagers.DHTProtocol do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.DHT.CQRS,
    name: __MODULE__

  alias Effusion.CQRS.Commands.{
    AddDHTNode,
    GetPeers
  }
  alias Effusion.CQRS.Events.{
    DHTNodeStarted,
    DHTNodeAdded,
    DHTEnabledForDownload,
    ReceivedPeersMatching,
    ReceivedNodesNearest
  }

  defstruct [
    primary_node_id: nil,
    routing_table: []
  ]

  def interested?(%DHTNodeStarted{node_id: node_id}) do
    {:start!, node_id}
  end

  def interested?(%DHTNodeAdded{primary_node_id: primary_node_id}) do
    {:continue!, primary_node_id}
  end

  def interested?(%DHTEnabledForDownload{node_id: primary_node_id}) do
    {:continue!, primary_node_id}
  end

  def interested?(%ReceivedPeersMatching{primary_node_id: primary_node_id}) do
    {:continue!, primary_node_id}
  end

  def interested?(%ReceivedNodesNearest{primary_node_id: primary_node_id}) do
    {:continue!, primary_node_id}
  end

  def handle(
    %__MODULE__{primary_node_id: primary_node_id} = dht,
    %DHTEnabledForDownload{info_hash: info_hash}
  ) do
    node_id = closest_node(dht, info_hash)
    %GetPeers{
      primary_node_id: primary_node_id,
      info_hash: info_hash,
      node_id: node_id
    }
  end

  def handle(
    %__MODULE__{
      primary_node_id: primary_node_id
    },
    %ReceivedNodesNearest{
      nodes: nodes
    }
  ) do
    Enum.map(nodes, fn {node_id, {host, port}} ->
      %AddDHTNode{
        primary_node_id: primary_node_id,
        node_id: node_id,
        host: host,
        port: port
      }
    end)
  end

  def closest_node(
    %__MODULE__{routing_table: routing_table},
    _info_hash
  ) do
    Enum.at(routing_table, 0)
  end

  def apply(
    %__MODULE__{primary_node_id: _primary_node_id} = dht,
    %DHTNodeStarted{node_id: node_id}
  ) do
    %__MODULE__{dht |
      primary_node_id: node_id
    }
  end

  def apply(
    %__MODULE__{routing_table: routing_table} = dht,
    %DHTNodeAdded{node_id: node_id}
  ) do
    %__MODULE__{dht |
      routing_table: [node_id | routing_table]
    }
  end
end
