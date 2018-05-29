defmodule Effusion.PWP.Messages do
  alias Effusion.PWP.Messages.Handshake
  @moduledoc """
    Encode and decode Peer Wire Protocol (PWP) messages.
  """

  defguard is_uint32(i) when i in 0..4294967295
  defguard is_all_uint32(a, b) when is_uint32(a) and is_uint32(b)
  defguard is_all_uint32(a, b, c) when is_uint32(a) and is_uint32(b) and is_uint32(c)
  defguard is_under_max_bitfield_size(s) when is_uint32(byte_size(s) + 1)
  defguard is_under_max_block_size(s) when is_uint32(byte_size(s) + 9)

  @doc """
  Decodes a binary into a peer wire protocol message.

  Incoming binaries are assumed *not* to have the four-byte message
  length at the beginning of the binary. Set the "packet" option on
  a `gen_tcp` socket to `4` so that the socket validates and handles the
  message length for you.

  ## Examples

      iex> Effusion.PWP.Messages.decode(<<>>)
      {:ok, :keepalive}

      iex> Effusion.PWP.Messages.decode(<<0>>)
      {:ok, :choke}

      iex> Effusion.PWP.Messages.decode(<<1>>)
      {:ok, :unchoke}

      iex> Effusion.PWP.Messages.decode(<<2>>)
      {:ok, :interested}

      iex> Effusion.PWP.Messages.decode(<<3>>)
      {:ok, :uninterested}

      iex> Effusion.PWP.Messages.decode(<<4, 4201 :: 32>>)
      {:ok, {:have, 4201}}

      iex> Effusion.PWP.Messages.decode(<<5, 0b11110001>>)
      {:ok, {:bitfield, <<241>>}}

      iex> Effusion.PWP.Messages.decode(<<6, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, {:request, %{index: 4201, offset: 69, size: 12}}}

      iex> Effusion.PWP.Messages.decode(<<7, 4201 :: 32, 69 :: 32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>)
      {:ok, {:piece, %{index: 4201, offset: 69, data: <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>}}}

      iex> Effusion.PWP.Messages.decode(<<8, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, {:cancel, %{index: 4201, offset: 69, size: 12}}}

      iex> Effusion.PWP.Messages.decode(<<42>>)
      {:error, :invalid}

      iex> Effusion.PWP.Messages.decode(<<42, "with payload">>)
      {:error, :invalid}

      iex> Effusion.PWP.Messages.decode(<<0, "this shouldn't be here">>)
      {:error, :no_payload_allowed}
  """
  def decode(b)

  def decode(<<>>), do: {:ok, :keepalive}
  def decode(<<0>>), do: {:ok, :choke}
  def decode(<<1>>), do: {:ok, :unchoke}
  def decode(<<2>>), do: {:ok, :interested}
  def decode(<<3>>), do: {:ok, :uninterested}

  def decode(<<4, index :: 32>>) do
    {:ok, {:have, index}}
  end

  def decode(<<5, rest :: binary>>) do
    {:ok, {:bitfield, rest}}
  end

  def decode(<<6, index :: 32, offset :: 32, size :: 32>>) do
    {:ok, {:request, %{index: index, offset: offset, size: size}}}
  end

  def decode(<<7, index :: 32, offset :: 32, data :: binary>>) do
    {:ok, {:piece, %{index: index, offset: offset, data: data}}}
  end

  def decode(<<8, index :: 32, offset :: 32, size :: 32>>) do
    {:ok, {:cancel, %{index: index, offset: offset, size: size}}}
  end

  def decode(<<19, "BitTorrent protocol", _rest::binary>> = msg), do: Handshake.decode(msg)

  def decode(<<id, rest :: binary>>) when id in 0..3 and bit_size(rest) > 0 do
    {:error, :no_payload_allowed}
  end

  def decode(<<_, _rest :: binary>>) do
    {:error, :invalid}
  end

  @doc """
  Encode a peer wire protocol message into a binary.

  ## Examples

      iex> Effusion.PWP.Messages.encode(:notamessage)
      {:error, {:unknown_message, :notamessage}}

      iex> Effusion.PWP.Messages.encode(:keepalive)
      {:ok, <<>>}

      iex> Effusion.PWP.Messages.encode(:choke)
      {:ok, <<0>>}

      iex> Effusion.PWP.Messages.encode(:unchoke)
      {:ok, <<1>>}

      iex> Effusion.PWP.Messages.encode(:interested)
      {:ok, <<2>>}

      iex> Effusion.PWP.Messages.encode(:uninterested)
      {:ok, <<3>>}

      iex> Effusion.PWP.Messages.encode({:have, 690042})
      {:ok, <<4, 690042 :: 32>>}

      iex> Effusion.PWP.Messages.encode({:bitfield, <<0b0000_0000>>})
      {:ok, <<5, 0>>}

      iex> Effusion.PWP.Messages.encode({:request, 420, 69, 666})
      {:ok, <<6, 0, 0, 1, 164, 0, 0, 0, 69, 0, 0, 2, 154>>}

      iex> Effusion.PWP.Messages.encode({:piece, 420, 69, <<1, 2, 3, 4, 5>>})
      {:ok, <<7, 0, 0, 1, 164, 0, 0, 0, 69, 1, 2, 3, 4, 5>>}

      iex> Effusion.PWP.Messages.encode({:cancel, 420, 69, 666})
      {:ok, <<8, 0, 0, 1, 164, 0, 0, 0, 69, 0, 0, 2, 154>>}

  """
  def encode(m)

  def encode(:keepalive), do: {:ok, <<>>}
  def encode(:choke), do: {:ok, <<0>>}
  def encode(:unchoke), do: {:ok, <<1>>}
  def encode(:interested), do: {:ok, <<2>>}
  def encode(:uninterested), do: {:ok, <<3>>}
  def encode({:have, index}) when is_uint32(index), do: {:ok, <<4, index :: 32>>}
  def encode({:bitfield, val}) when is_binary(val) and is_under_max_bitfield_size(val), do: {:ok, <<5>> <> val}
  def encode({:request, i, o, s}) when is_all_uint32(i, o, s), do: {:ok, <<6, i :: 32, o :: 32, s :: 32>>}
  def encode({:piece, i, o, b}) when is_all_uint32(i, o) and is_binary(b) and is_under_max_block_size(b), do: {:ok, <<7, i :: 32, o :: 32>> <> b}
  def encode({:cancel, i, o, s}) when is_all_uint32(i, o, s), do: {:ok, <<8, i :: 32, o :: 32, s :: 32>>}
  def encode({:handshake, peer_id, info_hash}), do: Handshake.encode(peer_id, info_hash)

  def encode(m), do: {:error, {:unknown_message, m}}
end
