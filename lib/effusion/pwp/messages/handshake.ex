alias Effusion.{PeerId, InfoHash}

defmodule Effusion.PWP.Messages.Handshake do
  @moduledoc """
  Encode and decode peer handshake messages.
  """

  @protocol_name <<"BitTorrent protocol" :: utf8>>
  @protocol_name_size <<19 :: integer-size(8)>>
  @reserved_bytes <<0, 0, 0, 0, 0, 0, 0, 0>>

  @typedoc """
  A binary encoded handshake message.
  """
  @type handshake_binary :: <<_::544>>

  @typedoc """
  The reserved portion of a handshake. Ignored by Effusion.
  """
  @type reserved_bytes :: <<_::64>>

  @typedoc """
  Reasons that decoding a handshake may fail.
  """
  @type decode_failure_reason :: :malformed_handshake

  @doc """
  Extracts the peer id, info hash, and reserved bytes from
  a handshake packet binary.
  """
  @spec decode(handshake_binary) :: {:ok, PeerId.t, InfoHash.t, reserved_bytes} | {:error, decode_failure_reason}
  def decode(handshake) do
    case handshake do
      <<@protocol_name_size,
      @protocol_name,
      reserved :: bytes-size(8),
      info_hash :: bytes-size(20),
      peer_id :: bytes-size(20)>> -> {:ok, peer_id, info_hash, reserved}
      _ -> {:error, :malformed_handshake}
    end
  end

  @doc """
  Builds a handshake packet binary for the info hash and peer id.
  """
  @spec encode(PeerId.t, InfoHash.t) :: handshake_binary
  def encode(peer_id, info_hash)  do
    <<@protocol_name_size,
    @protocol_name,
    @reserved_bytes,
    info_hash :: bytes-size(20),
    peer_id :: bytes-size(20) >>
  end
end
