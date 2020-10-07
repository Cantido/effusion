defmodule Effusion.CQRS.Contexts.Peers do
  alias Effusion.CQRS.Application
  alias Effusion.CQRS.Commands.{
    AddPeer,
    HandleHandshake,
    RemoveConnectedPeer
  }
  def add(info_hash, peer_id, host, port, from) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %AddPeer{
      internal_peer_id: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      from: from}
    |> Application.dispatch()
  end

  def successful_handshake(info_hash, peer_id, host, port, initiated_by, extensions) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %HandleHandshake{
      internal_peer_id: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      initiated_by: initiated_by,
      extensions: extensions}
    |> Application.dispatch()
  end

  def disconnected(info_hash, peer_id, host, port, reason) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    %RemoveConnectedPeer{
      internal_peer_id: "#{info_hash}:#{host}:#{port}",
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      reason: reason}
    |> Application.dispatch()
  end
end
