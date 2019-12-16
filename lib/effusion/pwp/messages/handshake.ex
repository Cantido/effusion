defmodule Effusion.PWP.Messages.Handshake do
  @moduledoc """
  Encode and decode peer handshake messages.
  """

  @protocol_name <<"BitTorrent protocol"::utf8>>
  @protocol_name_size <<19::integer-size(8)>>

  @doc """
  Extracts the peer id, info hash, and reserved bytes from
  a handshake packet binary.
  """
  def decode(
        <<@protocol_name_size, @protocol_name, reserved::bytes-size(8), info_hash::bytes-size(20),
          peer_id::bytes-size(20)>>
      ) do
    {:ok, {:handshake, peer_id, info_hash, decode_reserved(reserved)}}
  end

  def decode(_) do
    {:error, :malformed_handshake}
  end

  defp decode_reserved(<<0, 0, 0, 0, 0, 0, 0, 0b00000100>>) do
    [:fast]
  end

  defp decode_reserved(_reserved) do
    []
  end

  @doc """
  Builds a handshake packet binary for the info hash and peer id.
  """
  def encode(peer_id, info_hash, extensions \\ []) do
    @protocol_name_size <>
      @protocol_name <>
      encode_reserved(extensions) <>
      <<info_hash::bytes-size(20)>> <>
      <<peer_id::bytes-size(20)>>
  end

  defp encode_reserved([:fast]) do
    <<0, 0, 0, 0, 0, 0, 0, 0b00000100>>
  end

  defp encode_reserved([]) do
    <<0, 0, 0, 0, 0, 0, 0, 0>>
  end
end
