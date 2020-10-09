defmodule Effusion.CQRS.Contexts.Peers do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddPeer,
    SendHandshake,
    HandleHandshake,
    RemoveConnectedPeer
  }
  def add(peer_uuid, info_hash, peer_id, host, port, from) do
    %AddPeer{
      peer_uuid: peer_uuid,
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from}
    |> CQRS.dispatch()
  end

  def send_handshake(peer_uuid, info_hash, peer_id, initiated_by) do
    %SendHandshake{
      peer_uuid: peer_uuid,
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      our_peer_id: Application.fetch_env!(:effusion, :peer_id),
      our_extensions: Application.fetch_env!(:effusion, :enabled_extensions),
      initiated_by: initiated_by,
    }
    |> CQRS.dispatch()
  end

  def handle_handshake(peer_uuid, info_hash, peer_id, host, port, initiated_by, extensions) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %HandleHandshake{
      peer_uuid: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      initiated_by: initiated_by,
      extensions: extensions}
    |> CQRS.dispatch()
  end

  def disconnected(peer_uuid, info_hash, peer_id, reason) do
    %RemoveConnectedPeer{
      peer_uuid: peer_uuid,
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      reason: reason}
    |> CQRS.dispatch()
  end
end
