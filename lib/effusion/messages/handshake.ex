defmodule Effusion.Messages.Handshake do
  @moduledoc """
  Encode and decode peer handshake messages.
  """

  @protocol_name <<"BitTorrent protocol" :: utf8>>
  @protocol_name_size <<19 :: integer-size(8)>>
  @reserved_bytes <<0, 0, 0, 0, 0, 0, 0, 0>>

  @doc """
  Extracts the peer id, info hash, and reserved bytes from
  a handshake packet binary.
  """
  @spec decode(<<_::544>>) :: {:ok, <<_::160>>, <<_::160>>, <<_::64>>} | :error
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

  @doc """
  Builds a handshake packet binary for the info hash and peer id.
  """
  def encode(info_hash, peer_id)  do
    <<@protocol_name_size,
    @protocol_name,
    @reserved_bytes,
    info_hash :: bytes-size(20),
    peer_id :: bytes-size(20) >>
  end
end
