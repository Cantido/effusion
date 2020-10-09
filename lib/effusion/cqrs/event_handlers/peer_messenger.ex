defmodule Effusion.CQRS.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.TCP.Connection
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
    SendingHave,
    SuccessfulHandshake
  }
  import Ecto.Query
  require Logger

  def handle(
    %AttemptingToConnect{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, host: host, port: port},
    _metadata
  ) do

    Logger.debug "**** CQRS is opening a connection to #{host}:#{port}"

    {:ok, host} = :inet.parse_address(String.to_charlist(host))
    info_hash = Effusion.Hash.decode(info_hash)

    Connection.connect({{host, port}, info_hash, peer_id, peer_uuid})
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
    Connection.send_pwp_message(
      peer_uuid,
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

    :ok = Connection.send_pwp_message(
      peer_uuid,
      {:handshake, our_peer_id, decoded_info_hash, our_extensions}
    )

    {:ok, {:handshake, their_peer_id, their_info_hash, their_extensions}} =
      Connection.recv_handshake(peer_uuid)

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
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id
    },
    _metadata
  ) do
    Connection.handshake_successful(peer_uuid)
  end

  def handle(
    %SendingHave{peer_uuid: peer_uuid, index: index},
    _metadata
  ) when is_number(index) and index >= 0 do
    Connection.send_pwp_message(peer_uuid, {:have, index})
  end

  def handle(
    %InterestedSent{peer_uuid: peer_uuid},
    _metadata
  ) do
    Connection.send_pwp_message(peer_uuid, :interested)
  end

  def handle(
    %BlockRequested{peer_uuid: peer_uuid, index: index, offset: offset, size: size},
    _metadata
  ) do
    Connection.send_pwp_message(peer_uuid, {:request, index, offset, size})
  end

  def handle(
    %RequestCancelled{peer_uuid: peer_uuid, index: index, offset: offset, size: size},
    _metadata
  ) do
    Connection.send_pwp_message(peer_uuid, {:cancel, index, offset, size})
  end

  def handle(
    %BitfieldSent{peer_uuid: peer_uuid, bitfield: bitfield},
    _metadata
  ) do
    {:ok, decoded_bitfield} = Base.decode16(bitfield)
    Connection.send_pwp_message(peer_uuid, {:bitfield, decoded_bitfield})
  end
end
