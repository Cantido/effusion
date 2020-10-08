defmodule Effusion.CQRS.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

    alias Effusion.PWP.TCP.Connection
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.CQRS.Events.{
    BlockStored,
    AttemptingToConnect,
    PieceHashSucceeded,
    InterestedSent,
    BlockRequested
  }
  import Ecto.Query
  require Logger

  def handle(
    %PieceHashSucceeded{info_hash: info_hash, index: index},
    _metadata
  ) do
    Logger.debug("***** Piece hash succeeded for piece #{index}, sending :have")
    ConnectionRegistry.btp_broadcast(Effusion.Hash.decode(info_hash), {:have, index})

    :ok
  end

  def handle(
    %InterestedSent{info_hash: info_hash, peer_id: peer_id},
    _metadata
  ) do
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, :interested)

    :ok
  end

  def handle(
    %BlockRequested{info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size},
    _metadata
  ) do
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:request, index, offset, size})

    :ok
  end

  def handle(
    %AttemptingToConnect{info_hash: info_hash, peer_id: peer_id, host: host, port: port},
    _metadata
  ) do

    Logger.debug "**** CQRS is opening a connection to #{host}:#{port}"

    {:ok, host} = :inet.parse_address(String.to_charlist(host))
    info_hash = Effusion.Hash.decode(info_hash)

    Connection.connect({{host, port}, info_hash, peer_id})

    :ok
  end

  def handle(
    %BlockStored{from: from, index: index, offset: offset} = event,
    _metadata
  ) do
    Logger.debug("**** CQRS is writing block #{index}-#{offset} to the DB")

    block = Map.take(event, [:info_hash, :index, :offset, :data])
    |> Map.update!(:info_hash, &Effusion.Hash.decode/1)

    Effusion.BTP.Request.cancel(block, from)
    |> Enum.uniq()
    |> Enum.each(fn {peer_id, index, offset, size} ->
      Effusion.PWP.ConnectionRegistry.btp_send(block.info_hash, peer_id, {:cancel, index, offset, size})
    end)

    peer_request_query =
      from request in Effusion.BTP.Request,
      join: peer in assoc(request, :peer),
      where: peer.peer_id == ^from
    peer_request_count = Effusion.Repo.aggregate(peer_request_query, :count, :peer_id)

    max_requests = Application.get_env(:effusion, :max_requests_per_peer)

    if peer_request_count <= max_requests / 2 do
      Effusion.PWP.ProtocolHandler.next_request_from_peer(block.info_hash, from, max_requests)
    end

    :ok
  end
end
