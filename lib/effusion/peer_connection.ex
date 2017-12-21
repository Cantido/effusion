require Logger

defmodule Effusion.PeerConnection do
  @protocol_name "BitTorrent protocol"
  @protocol_name_size <<19>>
  @reserved_bytes <<0 :: size(64)>>

  def serve(socket) do
    with {:ok, data} <- :gen_tcp.recv(socket, 0),
         <<@protocol_name_size,
         @protocol_name,
         _reserved :: size(64),
         info_hash :: size(160),
         peer_id :: size(160)>> <- data
    do
      Logger.info ("Handshake from peer_id #{inspect(peer_id)} for info_hash #{inspect(info_hash)}")
      :gen_tcp.send(socket, handshake(info_hash(), peer_id()))
      :gen_tcp.shutdown(socket, :read_write)
    else
      _err -> :gen_tcp.shutdown(socket, :read_write)
    end
  end

  defp info_hash do
    <<0 :: size(160)>>
  end

  defp peer_id do
    <<0 :: size(160)>>
  end

  defp handshake(info_hash, peer_id) do
    @protocol_name_size <>
    @protocol_name <>
    @reserved_bytes <>
    info_hash <>
    peer_id
  end
end
