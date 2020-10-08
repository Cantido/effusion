defmodule Effusion.CQRS.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

    alias Effusion.PWP.TCP.Connection
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    PieceHashSucceeded,
    InterestedSent,
    BlockRequested,
    RequestCancelled,
    BitfieldSent
  }
  import Ecto.Query
  require Logger

  def handle(
    %PieceHashSucceeded{info_hash: info_hash, index: index},
    _metadata
  ) do
    Logger.debug("***** Piece hash succeeded for piece #{index}, sending :have")
    ConnectionRegistry.btp_broadcast(Effusion.Hash.decode(info_hash), {:have, index})
  end

  def handle(
    %InterestedSent{info_hash: info_hash, peer_id: peer_id},
    _metadata
  ) do
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, :interested)
  end

  def handle(
    %BlockRequested{info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size},
    _metadata
  ) do
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:request, index, offset, size})
  end

  def handle(
    %AttemptingToConnect{info_hash: info_hash, peer_id: peer_id, host: host, port: port},
    _metadata
  ) do

    Logger.debug "**** CQRS is opening a connection to #{host}:#{port}"

    {:ok, host} = :inet.parse_address(String.to_charlist(host))
    info_hash = Effusion.Hash.decode(info_hash)

    Connection.connect({{host, port}, info_hash, peer_id})
  end

  def handle(
    %RequestCancelled{info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size},
    _metadata
  ) do
    Effusion.PWP.ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:cancel, index, offset, size})
  end

  def handle(
    %BitfieldSent{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield},
    _metadata
  ) do
    {:ok, bitfield} = Base.decode16(bitfield)
    Effusion.PWP.ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:bitfield, bitfield})
  end
end
