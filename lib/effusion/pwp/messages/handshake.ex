defmodule Effusion.PWP.Messages.Handshake do
  alias Effusion.PWP.Socket

  @moduledoc """
  Encode and decode peer handshake messages.
  """

  @protocol_name <<"BitTorrent protocol"::utf8>>
  @protocol_name_size <<19::integer-size(8)>>
  @reserved_bytes <<0, 0, 0, 0, 0, 0, 0, 0>>

  @doc """
  Extracts the peer id, info hash, and reserved bytes from
  a handshake packet binary.
  """
  def decode(
        <<@protocol_name_size, @protocol_name, reserved::bytes-size(8), info_hash::bytes-size(20),
          peer_id::bytes-size(20)>>
      ) do
    {:ok, {:handshake, peer_id, info_hash, reserved}}
  end

  def decode(_) do
    {:error, :malformed_handshake}
  end

  @doc """
  Builds a handshake packet binary for the info hash and peer id.
  """
  def encode(peer_id, info_hash) do
    <<@protocol_name_size, @protocol_name, @reserved_bytes, info_hash::bytes-size(20),
      peer_id::bytes-size(20)>>
  end


  def perform(socket, local_peer_id, expected_peer_id, local_info_hash) do
    with :ok <- Socket.send_msg(socket, {:handshake, local_peer_id, local_info_hash}),
         {:ok, hs = {:handshake, remote_peer_id, _, _}} <- Socket.recv(socket, 68),
         :ok <- validate(expected_peer_id, local_info_hash, hs),
         :ok <- :inet.setopts(socket, packet: 4) do
      {:ok, socket, remote_peer_id}
    else
      err -> err
    end
  end

  defp validate(expected_peer_id, local_info_hash, {:handshake, remote_peer_id, remote_info_hash, _reserved}) do
    cond do
      local_info_hash != remote_info_hash ->
        {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}

      expected_peer_id != nil and expected_peer_id != remote_peer_id ->
        {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}

      true ->
        :ok
    end
  end
end
