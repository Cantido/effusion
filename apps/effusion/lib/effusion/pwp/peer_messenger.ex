defmodule Effusion.PWP.EventHandlers.PeerMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.TCP.Connection
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.PWP.Handshake.Commands.HandleHandshake
  alias Effusion.PWP.Messages.Outgoing.Events.{
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }
  alias Effusion.PWP.Handshake.Events.SuccessfulHandshake
  alias Effusion.PWP.Connection.Events.AttemptingToConnect
  alias Effusion.PWP.Handshake.Events.SendingHandshake
  require Logger

  def handle(
    %AttemptingToConnect{peer_uuid: peer_uuid, host: host, port: port},
    _metadata
  ) do
    Logger.debug "**** CQRS is opening a connection to #{host}:#{port}"
    {:ok, host} = :inet.parse_address(String.to_charlist(host))

    Connection.connect({{host, port}, peer_uuid})
  end

  def handle(
    %SendingHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: "them"
    },
    _metadata
  ) do
    Logger.debug("***** Sending handshake")
    info_hash = Effusion.Hash.decode(info_hash)
    our_peer_id = Effusion.Hash.decode(our_peer_id)
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
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: "us"
    },
    _metadata
  ) do
    Logger.debug("***** Sending handshake")
    decoded_info_hash = Effusion.Hash.decode(info_hash)
    our_peer_id = Effusion.Hash.decode(our_peer_id)

    :ok = Connection.send_pwp_message(
      peer_uuid,
      {:handshake, our_peer_id, decoded_info_hash, our_extensions}
    )

    {:ok, {:handshake, their_peer_id, their_info_hash, their_extensions}} =
      Connection.recv_handshake(peer_uuid)

    %HandleHandshake{
      peer_uuid: peer_uuid,
      info_hash: Effusion.Hash.encode(their_info_hash),
      peer_id: Effusion.Hash.encode(their_peer_id),
      initiated_by: "us",
      extensions: their_extensions}
    |> CQRS.dispatch()
  end


  def handle(
    %SuccessfulHandshake{
      peer_uuid: peer_uuid
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
    {:ok, decoded_bitfield} = Base.decode64(bitfield)
    Connection.send_pwp_message(peer_uuid, {:bitfield, decoded_bitfield})
  end

  def error(error, failed_event, _context) do
    Logger.warn("PeerMessenger failed to handle event #{inspect failed_event}, got error #{inspect error}. Skipping event.")
    :skip
  end
end
