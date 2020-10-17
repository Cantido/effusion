defmodule Effusion.CQRS.Aggregates.DHT do
  alias Effusion.CQRS.Commands.{
    StartDHTNode,
    EnableDHTForDownload
  }

  alias Effusion.CQRS.Events.{
    DHTNodeStarted,
    DHTEnabledForDownload
  }

  defstruct [
    info_hashes: MapSet.new(),
    node_id: nil
  ]

  def execute(
    %__MODULE__{node_id: nil},
    %StartDHTNode{node_id: node_id}
  ) do
    %DHTNodeStarted{node_id: node_id}
  end

  def execute(
    %__MODULE__{},
    %StartDHTNode{}
  ) do
    {:error, :node_already_started}
  end

  def execute(
    %__MODULE__{info_hashes: info_hashes},
    %EnableDHTForDownload{info_hash: info_hash, node_id: node_id}
  ) do
    if MapSet.member?(info_hashes, info_hash) do
      {:error, "DHT already enabled for info hash #{inspect info_hash}"}
    else
      %DHTEnabledForDownload{info_hash: info_hash, node_id: node_id}
    end
  end

  def apply(
    %__MODULE__{} = torrent,
    %DHTNodeStarted{node_id: node_id}
  ) do
    %__MODULE__{torrent|
      node_id: node_id
    }
  end

  def apply(
    %__MODULE__{info_hashes: info_hashes} = torrent,
    %DHTEnabledForDownload{info_hash: info_hash}
  ) do
    %__MODULE__{torrent|
      info_hashes: MapSet.put(info_hashes, info_hash)
    }
  end
end
