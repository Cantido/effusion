defmodule Effusion.CQRS.Contexts.Peers do
  def add(info_hash, peer_id, host, port, from) do
    %Effusion.CQRS.Commands.AddPeer{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: :connection}
    |> Effusion.CQRS.Application.dispatch()
  end

  def successful_handshake(info_hash, peer_id, host, port, initiated_by) do
    %Effusion.CQRS.Commands.HandleHandshake{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      initiated_by: initiated_by}
    |> Effusion.CQRS.Application.dispatch()
  end

  def disconnected(info_hash, peer_id, host, port, reason) do
    %Effusion.CQRS.Commands.RemoveConnectedPeer{
      info_hash: Effusion.Hash.encode(info_hash),
      peer_id: peer_id,
      host: to_string(:inet.ntoa(host)),
      port: port,
      reason: reason}
    |> Effusion.CQRS.Application.dispatch()
  end
end
