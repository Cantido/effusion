defmodule Effusion.CQRS.Contexts.Peers do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddPeer,
    SendHandshake,
    HandleHandshake,
    RemoveConnectedPeer
  }
  def add(info_hash, peer_id, host, port, from) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %AddPeer{
      peer_uuid: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      from: from}
    |> CQRS.dispatch()
  end

  def send_handshake(info_hash, peer_id, host, port, initiated_by) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))

    our_peer_id = Application.fetch_env!(:effusion, :peer_id)
    our_extensions = Application.fetch_env!(:effusion, :enabled_extensions)
    %SendHandshake{
      peer_uuid: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: initiated_by,
    }
    |> CQRS.dispatch()
  end

  def handle_handshake(info_hash, peer_id, host, port, initiated_by, extensions) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %HandleHandshake{
      peer_uuid: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      initiated_by: initiated_by,
      extensions: extensions}
    |> CQRS.dispatch()
  end

  def disconnected(info_hash, peer_id, host, port, reason) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %RemoveConnectedPeer{
      peer_uuid: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      reason: reason}
    |> CQRS.dispatch()
  end
end
