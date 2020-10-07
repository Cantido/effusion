defmodule Effusion.CQRS.Contexts.Peers do
  alias Effusion.CQRS.Application
  alias Effusion.CQRS.Commands.{
    AddPeer,
    HandleHandshake,
    RemoveConnectedPeer
  }
  def add(info_hash, peer_id, host, port, from) do
    %AddPeer{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: :connection}
    |> Application.dispatch()
  end

  def successful_handshake(info_hash, peer_id, host, port, initiated_by) do
    %HandleHandshake{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      initiated_by: initiated_by}
    |> Application.dispatch()
  end

  def disconnected(info_hash, peer_id, host, port, reason) do
    %RemoveConnectedPeer{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      reason: reason}
    |> Application.dispatch()
  end
end
