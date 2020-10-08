defmodule Effusion.CQRS.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.TCP.Connection
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.HandleHandshake
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    PieceHashSucceeded,
    InterestedSent,
    BlockRequested,
    RequestCancelled,
    BitfieldSent,
    SendingHandshake,
    SuccessfulHandshake
  }
  import Ecto.Query
  require Logger

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
    %SendingHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :them
    },
    _metadata
  ) do
    Logger.debug("***** Sending handshake")
    info_hash = Effusion.Hash.decode(info_hash)
    ConnectionRegistry.btp_send(
      info_hash,
      peer_id,
      {:handshake, our_peer_id, info_hash, our_extensions}
    )

    :ok
  end

  def handle(
    %SendingHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :us
    },
    _metadata
  ) do
    Logger.debug("***** Sending handshake")
    decoded_info_hash = Effusion.Hash.decode(info_hash)

    :ok = ConnectionRegistry.btp_send(
      decoded_info_hash,
      peer_id,
      {:handshake, our_peer_id, decoded_info_hash, our_extensions}
    )

    {:ok, {:handshake, their_peer_id, their_info_hash, their_extensions}} =
      Connection.recv_handshake(decoded_info_hash, peer_id)

    %HandleHandshake{
      peer_uuid: peer_uuid,
      info_hash: Effusion.Hash.encode(their_info_hash),
      peer_id: their_peer_id,
      initiated_by: :us,
      extensions: their_extensions}
    |> CQRS.dispatch()
  end


  def handle(
    %SuccessfulHandshake{
      info_hash: info_hash,
      peer_id: peer_id
    },
    _metadata
  ) do
    Connection.handshake_successful(Effusion.Hash.decode(info_hash), peer_id)
  end

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
    %RequestCancelled{info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size},
    _metadata
  ) do
    Effusion.PWP.ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:cancel, index, offset, size})
  end

  def handle(
    %BitfieldSent{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield},
    _metadata
  ) do
    Logger.debug("******* Sending bitfield")
    {:ok, decoded_bitfield} = Base.decode16(bitfield)
    Effusion.PWP.ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:bitfield, decoded_bitfield})
  end
end
