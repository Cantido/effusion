require Logger

defmodule Effusion.PeerConnection do
  def serve(socket) do
    with {:ok, data} <- :gen_tcp.recv(socket, 0),
         <<19,
         "BitTorrent protocol",
         _reserved :: size(64),
         info_hash :: size(160),
         peer_id :: size(160)>> <- data
    do
      Logger.info ("Handshake from peer_id #{inspect(peer_id)} for info_hash #{inspect(info_hash)}")
      :gen_tcp.send(socket, handshake())
      :gen_tcp.shutdown(socket, :read_write)
    else
      _err -> :gen_tcp.shutdown(socket, :read_write)
    end
  end

  defp handshake do
    name_size = <<19>>
    name = "BitTorrent protocol"
    reserved = <<0 :: size(64)>>
    info_hash = <<0 :: size(160)>>
    peer_id = <<0 :: size(160)>>

    name_size <> name <> reserved <> info_hash <> peer_id
  end
end
