defmodule Effusion.CQRS.Contexts.Peers do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddPeer,
    AddOpenedPeerConnection,
    HandleFailedConnectionAttempt,
    HandleHandshake,
    RemoveConnectedPeer,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  }

  def add(peer_uuid, info_hash, host, port, from) do
    %AddPeer{
      peer_uuid: peer_uuid,
      expected_info_hash: Effusion.Hash.encode(info_hash),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from}
    |> CQRS.dispatch()
  end

  def add(peer_uuid, info_hash, peer_id, host, port, from) do
    %AddPeer{
      peer_uuid: peer_uuid,
      expected_info_hash: Effusion.Hash.encode(info_hash),
      expected_peer_id: Effusion.Hash.encode(peer_id),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from}
    |> CQRS.dispatch()
  end

  def add_opened_peer_connection(peer_uuid, host, port) do
    %AddOpenedPeerConnection{
      peer_uuid: peer_uuid,
      host: to_string(:inet.ntoa(host)),
      port: port
    }
    |> CQRS.dispatch()
  end

  def disconnected(peer_uuid, reason) do
    %RemoveConnectedPeer{
      peer_uuid: peer_uuid,
      reason: inspect(reason)}
    |> CQRS.dispatch()
  end

  def handle_failed_connection_attempt(peer_uuid, reason) do
    %HandleFailedConnectionAttempt{
      peer_uuid: peer_uuid,
      reason: reason
    }
    |> CQRS.dispatch()
  end

  def handle_message(peer_uuid, message, initiated_by \\ nil) do
    case message do
      {:handshake, peer_id, info_hash, extensions} ->
        %HandleHandshake{
          peer_uuid: peer_uuid,
          info_hash: Effusion.Hash.encode(info_hash),
          peer_id: Effusion.Hash.encode(peer_id),
          initiated_by: initiated_by,
          extensions: extensions}
      :choke -> %HandleChoke{peer_uuid: peer_uuid}
      :unchoke -> %HandleUnchoke{peer_uuid: peer_uuid}
      :interested -> %HandleInterested{peer_uuid: peer_uuid}
      :uninterested -> %HandleUninterested{peer_uuid: peer_uuid}
      {:have, index} -> %HandleHave{peer_uuid: peer_uuid, index: index}
      {:bitfield, bitfield} -> %HandleBitfield{peer_uuid: peer_uuid, bitfield: Base.encode64(bitfield)}
      {:request, block} -> %HandleRequest{peer_uuid: peer_uuid, index: block.index, offset: block.offset, size: block.size}
      {:cancel, block} -> %HandleCancel{peer_uuid: peer_uuid, index: block.index, offset: block.offset, size: block.size}
      {:piece, block} -> %HandlePiece{peer_uuid: peer_uuid, index: block.index, offset: block.offset, data: Base.encode64(block.data)}
    end
    |> CQRS.dispatch()
  end
end
