defmodule Effusion.PWP.Messages do
  @moduledoc """
    Encode and decode Peer Wire Protocol (PWP) messages.
  """

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
      {:ok, :have, 4201}

      iex> Effusion.PWP.Messages.decode(<<5, 0b11110001>>)
      {:ok, :bitfield, <<241>>}

      iex> Effusion.PWP.Messages.decode(<<6, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, :request, %{index: 4201, offset: 69, size: 12}}

      iex> Effusion.PWP.Messages.decode(<<7, 4201 :: 32, 69 :: 32, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>)
      {:ok, :piece, %{index: 4201, offset: 69, data: <<1, 2, 3, 4, 5, 6, 7, 8, 9, 0>>}}

      iex> Effusion.PWP.Messages.decode(<<8, 4201 :: 32, 69 :: 32, 12 :: 32>>)
      {:ok, :cancel, %{index: 4201, offset: 69, size: 12}}

      iex> Effusion.PWP.Messages.decode(<<42>>)
      {:error, :bad_message_id}

      iex> Effusion.PWP.Messages.decode(<<42, "with payload">>)
      {:error, :bad_message_id}

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
    {:ok, :have, index}
  end

  def decode(<<5, rest :: binary>>) do
    {:ok, :bitfield, rest}
  end

  def decode(<<6, index :: 32, offset :: 32, size :: 32>>) do
    {:ok, :request, %{index: index, offset: offset, size: size}}
  end

  def decode(<<7, index :: 32, offset :: 32, data :: binary>>) do
    {:ok, :piece, %{index: index, offset: offset, data: data}}
  end

  def decode(<<8, index :: 32, offset :: 32, size :: 32>>) do
    {:ok, :cancel, %{index: index, offset: offset, size: size}}
  end

  def decode(<<id, rest :: binary>>) when id in 0..3 and bit_size(rest) > 0 do
    {:error, :no_payload_allowed}
  end

  def decode(<<id, _rest :: binary>>) when id not in 0..8 do
    {:error, :bad_message_id}
  end

  def decode(<<_, _rest :: binary>>) do
    {:error, :invalid}
  end
end
