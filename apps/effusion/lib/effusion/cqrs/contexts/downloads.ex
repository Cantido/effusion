defmodule Effusion.CQRS.Contexts.Downloads do
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    StartDownload,
    StopDownload,
    StoreBlock
  }
  alias Effusion.CQRS.Application, as: CQRS

  def add(meta) do
    %AddTorrent{
      announce: meta.announce,
      announce_list: Map.get(meta, :announce_list),
      comment: Map.get(meta, :comment),
      created_by: meta.created_by,
      creation_date: meta.creation_date,
      info: encode(meta.info),
      info_hash: Effusion.Hash.encode(meta.info_hash)
    }
    |> CQRS.dispatch()
  end

  defp encode(info) do
    info
    |> Map.from_struct()
    |> Map.update!(:pieces, fn pieces ->
      Enum.map(pieces, &Effusion.Hash.encode/1)
    end)
  end

  def start(info_hash, block_size, max_requests_per_peer, max_half_open_connections, max_connections) do
    %StartDownload{
      info_hash: Effusion.Hash.encode(info_hash),
      block_size: block_size,
      max_requests_per_peer: max_requests_per_peer,
      max_half_open_connections: max_half_open_connections,
      max_connections: max_connections
    }
    |> CQRS.dispatch()
  end

  def stop(info_hash) do
    %StopDownload{
      info_hash: Effusion.Hash.encode(info_hash),
      tracker_event: "stopped"
    }
    |> CQRS.dispatch(consistency: :strong)
  end
end