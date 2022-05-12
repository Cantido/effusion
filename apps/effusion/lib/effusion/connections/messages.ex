defmodule Effusion.Messages do
  alias Effusion.Handshake

  @moduledoc """
    Encode and decode Peer Wire Protocol (PWP) messages.
  """

  defguardp is_uint32(i) when i in 0..4_294_967_295
  defguardp is_all_uint32(a, b) when is_uint32(a) and is_uint32(b)
  defguardp is_all_uint32(a, b, c) when is_uint32(a) and is_uint32(b) and is_uint32(c)
  defguardp is_under_max_bitfield_size(s) when is_uint32(byte_size(s) + 1)
  defguardp is_under_max_block_size(s) when is_uint32(byte_size(s) + 9)

  @doc """
  Decodes a binary into a peer wire protocol message.

  Incoming binaries are assumed *not* to have the four-byte message
  length at the beginning of the binary. Set the "packet" option on
  a `gen_tcp` socket to `4` so that the socket validates and handles the
  message length for you.

  ## Examples

      iex> Effusion.Messages.decode(<<>>)
      {:ok, :keepalive}

      iex> Effusion.Messages.decode(<<0>>)
      {:ok, :choke}

      iex> Effusion.Messages.decode(<<1>>)
      {:ok, :unchoke}

      iex> Effusion.Messages.decode(<<2>>)
      {:ok, :interested}

      iex> Effusion.Messages.decode(<<3>>)
      {:ok, :uninterested}

      iex> Effusion.Messages.decode(<<4, 4201 :: 32>>)
      {:ok, {:have, 4201}}

      iex> Effusion.Messages.decode(<<5, 0b11110001>>)
      {:ok, {:bitfield, <<241>>}}

      iex> Effusion.Messages.decode(<<6, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, {:request, %{index: 4201, offset: 69, size: 12}}}

      iex> Effusion.Messages.decode(<<7, 4201 :: 32, 69 :: 32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>)
      {:ok, {:piece, %{index: 4201, offset: 69, data: <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>}}}

      iex> Effusion.Messages.decode(<<8, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, {:cancel, %{index: 4201, offset: 69, size: 12}}}

  ### Fast Extension Messages

      iex> Effusion.Messages.decode(<<13, 0, 0, 0, 230>>)
      {:ok, {:suggest_piece, 230}}

      iex> Effusion.Messages.decode(<<14>>)
      {:ok, :have_all}

      iex> Effusion.Messages.decode(<<15>>)
      {:ok, :have_none}

      iex> Effusion.Messages.decode(<<16, 0, 0, 0, 230, 0, 0, 0, 60, 0, 0, 0, 200>>)
      {:ok, {:reject, %{index: 230, offset: 60, size: 200}}}

      iex> Effusion.Messages.decode(<<17, 0, 0, 0, 230>>)
      {:ok, {:allowed_fast, 230}}

  ### Invalid messages

      iex> Effusion.Messages.decode(<<42>>)
      {:error, :invalid}

      iex> Effusion.Messages.decode(<<42, "with payload">>)
      {:error, :invalid}

      iex> Effusion.Messages.decode(<<0, "this shouldn't be here">>)
      {:error, :no_payload_allowed}
  """
  def decode(b)

  def decode(<<>>), do: {:ok, :keepalive}
  def decode(<<0>>), do: {:ok, :choke}
  def decode(<<1>>), do: {:ok, :unchoke}
  def decode(<<2>>), do: {:ok, :interested}
  def decode(<<3>>), do: {:ok, :uninterested}

  def decode(<<4, index::32>>) do
    {:ok, {:have, index}}
  end

  def decode(<<5, rest::binary>>) do
    {:ok, {:bitfield, rest}}
  end

  def decode(<<6, index::32, offset::32, size::32>>) do
    {:ok, {:request, %{index: index, offset: offset, size: size}}}
  end

  def decode(<<7, index::32, offset::32, data::binary>>) do
    {:ok, {:piece, %{index: index, offset: offset, data: data}}}
  end

  def decode(<<8, index::32, offset::32, size::32>>) do
    {:ok, {:cancel, %{index: index, offset: offset, size: size}}}
  end

  def decode(<<13, index::32>>) do
    {:ok, {:suggest_piece, index}}
  end

  def decode(<<14>>) do
    {:ok, :have_all}
  end

  def decode(<<15>>) do
    {:ok, :have_none}
  end

  def decode(<<16, index::32, offset::32, size::32>>) do
    {:ok, {:reject, %{index: index, offset: offset, size: size}}}
  end

  def decode(<<17, index::32>>) do
    {:ok, {:allowed_fast, index}}
  end

  def decode(msg = <<19, "BitTorrent protocol", _rest::binary>>), do: Handshake.decode(msg)

  def decode(<<id, rest::binary>>) when id in 0..3 and bit_size(rest) > 0 do
    {:error, :no_payload_allowed}
  end

  def decode(<<_, _rest::binary>>) do
    {:error, :invalid}
  end

  def payload_bytes_count(<<7, _index::32, _offset::32, data::binary>>), do: byte_size(data)
  def payload_bytes_count(_msg), do: 0

  @doc """
  Encode a peer wire protocol message into a binary.

  ## Examples

      iex> Effusion.Messages.encode(:notamessage)
      {:error, {:unknown_message, :notamessage}}

      iex> Effusion.Messages.encode(:keepalive)
      {:ok, <<>>}

      iex> Effusion.Messages.encode(:choke)
      {:ok, <<0>>}

      iex> Effusion.Messages.encode(:unchoke)
      {:ok, <<1>>}

      iex> Effusion.Messages.encode(:interested)
      {:ok, <<2>>}

      iex> Effusion.Messages.encode(:uninterested)
      {:ok, <<3>>}

      iex> Effusion.Messages.encode({:have, 690042})
      {:ok, <<4, 690042 :: 32>>}

      iex> Effusion.Messages.encode({:bitfield, <<0b0000_0100>>})
      {:ok, <<5, 0b0000_0100>>}

      iex> Effusion.Messages.encode({:request, 420, 69, 666})
      {:ok, <<6, 0, 0, 1, 164, 0, 0, 0, 69, 0, 0, 2, 154>>}

      iex> Effusion.Messages.encode({:piece, 420, 69, <<1, 2, 3, 4, 5>>})
      {:ok, <<7, 0, 0, 1, 164, 0, 0, 0, 69, 1, 2, 3, 4, 5>>}

      iex> Effusion.Messages.encode({:cancel, 420, 69, 666})
      {:ok, <<8, 0, 0, 1, 164, 0, 0, 0, 69, 0, 0, 2, 154>>}

  ### Fast Extension messages

      iex> Effusion.Messages.encode({:suggest_piece, 230})
      {:ok, <<13, 0, 0, 0, 230>>}

      iex> Effusion.Messages.encode(:have_all)
      {:ok, <<14>>}

      iex> Effusion.Messages.encode(:have_none)
      {:ok, <<15>>}

      iex> Effusion.Messages.encode({:reject, 230, 60, 200})
      {:ok, <<16, 0, 0, 0, 230, 0, 0, 0, 60, 0, 0, 0, 200>>}

      iex> Effusion.Messages.encode({:allowed_fast, 230})
      {:ok, <<17, 0, 0, 0, 230>>}

  """
  def encode(m)

  def encode(:keepalive), do: {:ok, <<>>}
  def encode(:choke), do: {:ok, <<0>>}
  def encode(:unchoke), do: {:ok, <<1>>}
  def encode(:interested), do: {:ok, <<2>>}
  def encode(:uninterested), do: {:ok, <<3>>}
  def encode({:have, index}) when is_uint32(index), do: {:ok, <<4, index::32>>}

  def encode({:bitfield, val})
      when is_bitstring(val) and is_under_max_bitfield_size(val) do
    {:ok, <<5>> <> right_pad_bitstring_to_bytes(val)}
  end

  def encode({:request, i, o, s}) when is_all_uint32(i, o, s),
    do: {:ok, <<6, i::32, o::32, s::32>>}

  def encode({:piece, i, o, b})
      when is_all_uint32(i, o) and is_binary(b) and is_under_max_block_size(b),
      do: {:ok, <<7, i::32, o::32>> <> b}

  def encode({:cancel, i, o, s}) when is_all_uint32(i, o, s),
    do: {:ok, <<8, i::32, o::32, s::32>>}

  def encode({:cancel, %{index: i, offset: o, size: s}}) when is_all_uint32(i, o, s),
    do: encode({:cancel, i, o, s})

  def encode({:suggest_piece, index}) when is_uint32(index),
    do: {:ok, <<13, index::32>>}

  def encode(:have_all), do: {:ok, <<14>>}

  def encode(:have_none), do: {:ok, <<15>>}

  def encode({:reject, i, o, s}) when is_all_uint32(i, o, s),
    do: {:ok, <<16, i::32, o::32, s::32>>}

  def encode({:allowed_fast, index}) when is_uint32(index),
    do: {:ok, <<17, index::32>>}

  def encode({:handshake, peer_id, info_hash}) do
    {:ok, Handshake.encode(peer_id, info_hash)}
  end

  def encode({:handshake, peer_id, info_hash, extensions}) do
    {:ok, Handshake.encode(peer_id, info_hash, extensions)}
  end

  def encode(m), do: {:error, {:unknown_message, m}}

  defp right_pad_bitstring_to_bytes(bits) when is_bitstring(bits) do
    extra_bits_needed = byte_size(bits) * 8 - bit_size(bits)
    padding = <<0::size(extra_bits_needed)>>
    <<bits::bitstring, padding::bitstring>>
  end

  def type(msg) when is_atom(msg), do: msg
  def type(msg) when is_tuple(msg), do: elem(msg, 0)

  def size(msg) do
    with {:ok, encoded} <- encode(msg) do
      byte_size(encoded)
    end
  end
end
