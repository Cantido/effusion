defmodule Effusion.PWP.Messages.Handshake do
  use Bitwise

  @moduledoc """
  Encode and decode peer handshake messages.
  """

  @protocol_name <<"BitTorrent protocol"::utf8>>
  @protocol_name_size <<19::integer-size(8)>>
  @extension_masks %{
    dht:  0x0000_0000_0000_0001,
    fast: 0x0000_0000_0000_0004
  }

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

  defp decode_reserved(<<reserved_int::64>>) do
    Enum.reduce(@extension_masks, [], fn {name, mask}, acc ->
      if (reserved_int ||| mask) == reserved_int do
        [name | acc]
      else
        acc
      end
    end)
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

  defp encode_reserved(extensions) when is_list(extensions) do
    reserved_int =
      Enum.reduce(extensions, 0, fn extension, acc ->
        acc ||| @extension_masks[extension]
      end)

    <<reserved_int::64>>
  end

  defp encode_reserved([]) do
    <<0, 0, 0, 0, 0, 0, 0, 0>>
  end
end
