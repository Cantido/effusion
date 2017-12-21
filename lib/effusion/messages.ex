defmodule Effusion.Messages.Handshake do
  @protocol_name <<"BitTorrent protocol" :: utf8>>
  @protocol_name_size <<19 :: integer-size(8)>>
  @reserved_bytes <<0, 0, 0, 0, 0, 0, 0, 0>>

  def decode(handshake) do
    case handshake do
      <<@protocol_name_size,
      @protocol_name,
      reserved :: bytes-size(8),
      info_hash :: bytes-size(20),
      peer_id :: bytes-size(20)>> -> {:ok, peer_id, info_hash, reserved}
      _ -> :error
    end
  end

  def encode(info_hash, peer_id)  do
    <<@protocol_name_size,
    @protocol_name,
    @reserved_bytes,
    info_hash :: bytes-size(20),
    peer_id :: bytes-size(20) >>
  end
end
