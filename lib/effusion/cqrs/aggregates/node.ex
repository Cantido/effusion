defmodule Effusion.CQRS.Aggregates.Node do
  alias Effusion.CQRS.Commands.{
    AddDHTNode,
    StartDHTNode,
    GetPeers,
    IssueToken,
    RefreshNode,
    HandlePeersMatching
  }
  alias Effusion.CQRS.Events.{
    DHTNodeStarted,
    DHTNodeAdded,
    GettingPeers,
    TokenIssued,
    NodeRefreshed,
    ReceivedPeersMatching
  }
  alias Effusion.DHT

  defstruct [
    primary_node_id: nil,
    node_id: nil,
    host: nil,
    port: nil,
    transactions: %{},
    issued_token: nil,
    recieved_token: nil,
    last_contacted: nil
  ]

  def execute(
    %__MODULE__{node_id: nil},
    %StartDHTNode{node_id: node_id}
  ) do
    %DHTNodeStarted{node_id: node_id}
  end

  def execute(
    %__MODULE__{node_id: nil},
    %AddDHTNode{
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port
    }
  ) do
    %DHTNodeAdded{
      primary_node_id: primary_node_id,
      node_id: node_id,
      host: host,
      port: port
    }
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

  def execute(
    %__MODULE__{
      primary_node_id: primary_node_id,
      node_id: node_id
    },
    %IssueToken{token: token, expires_at: expires_at, info_hash: info_hash}
  ) do
    %TokenIssued{
      primary_node_id: primary_node_id,
      node_id: node_id,
      info_hash: info_hash,
      token: token,
      expires_at: expires_at
    }
  end

  def execute(
    %__MODULE__{node_id: node_id},
    %RefreshNode{last_contacted: last_contacted}
  ) do
    %NodeRefreshed{
      node_id: node_id,
      last_contacted: last_contacted
    }
  end

  def execute(
    %__MODULE__{
      primary_node_id: primary_node_id,
      transactions: transactions
    },
    %HandlePeersMatching{
      transaction_id: transaction_id,
      node_id: node_id,
      token: token,
      peers: peers
    }
  ) do
    if Map.has_key?(transactions, transaction_id) do
      info_hash = Map.fetch!(transactions, transaction_id)
      %ReceivedPeersMatching{
        primary_node_id: primary_node_id,
        transaction_id: transaction_id,
        info_hash: info_hash,
        node_id: node_id,
        token: token,
        peers: peers
      }
    else
      {:error, "transaction ID not found"}
    end
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
    %__MODULE__{transactions: transactions} = node,
    %GettingPeers{
      transaction_id: transaction_id,
      info_hash: info_hash
    }
  ) do
    %__MODULE__{node |
      transactions: Map.put(transactions, transaction_id, info_hash)
    }
  end

  def apply(
    %__MODULE__{} = node,
    %TokenIssued{token: token}
  ) do
    %__MODULE__{node | issued_token: token}
  end

  def apply(
    %__MODULE__{} = node,
    %NodeRefreshed{last_contacted: last_contacted}
  ) do
    %__MODULE__{node | last_contacted: last_contacted}
  end

  def apply(
    %__MODULE__{} = node,
    %ReceivedPeersMatching{}
  ) do
    node
  end
end
