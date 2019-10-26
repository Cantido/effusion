defmodule Effusion.BTP.Peer do
  import Effusion.Hash

  @moduledoc """
  Functions for manipulating a member of a BitTorrent swarm.
  """

  @doc """
  Returns `true` if `term` is a 20-byte binary peer ID; `false` otherwise.

  Allowed in guard tests.

  ## Examples

      iex> Effusion.BTP.Peer.is_peer_id("12345678901234567890")
      true

      iex> Effusion.BTP.Peer.is_peer_id("1234567890")
      false
  """
  defguard is_peer_id(term) when is_binary(term) and byte_size(term) == 20

  defstruct [
    :address,
    :peer_id,
    :info_hash,
    remote_peer_id: nil,
    failcount: 0,
    peer_choking: true,
    peer_interested: false,
    am_choking: true,
    am_interested: false,
    has: IntSet.new()
  ]

  @doc """
  Create a new peer data structure.

  Note that the `peer_id` argument is our *local* peer ID.
  To set the ID of the remote peer, see `set_remote_peer_id/2`.
  """
  def new({_host, port} = address, peer_id, info_hash)
      when is_peer_id(peer_id) and is_hash(info_hash) and port > 0 do
    %__MODULE__{
      address: address,
      peer_id: peer_id,
      info_hash: info_hash
    }
  end

  @doc """
  Set the 20-byte peer ID value that identifies the remote peer.
  """
  def set_remote_peer_id(p = %__MODULE__{peer_id: peer_id}, remote_peer_id)
      when (is_peer_id(remote_peer_id) or remote_peer_id == nil) and peer_id != remote_peer_id do
    Map.put(p, :remote_peer_id, remote_peer_id)
  end

  def inc_fail_count(peer = %__MODULE__{}) when is_map(peer) do
    Map.update(peer, :failcount, 0, &(&1+1))
  end

  def dec_fail_count(peer = %__MODULE__{}) when is_map(peer) do
    Map.update(peer, :failcount, 0, &(&1-1))
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message, and return the updated peer
  along with the messages to send back to the remote peer.
  """
  def recv(peer, message)

  def recv(p = %__MODULE__{}, {:bitfield, b}) when is_map(p) do
    p =
      p
      |> Map.put(:has, IntSet.new(b))
      |> Map.put(:am_choking, false)
      |> Map.put(:am_interested, true)

    {p, [:interested, :unchoke]}
  end

  def recv(p = %__MODULE__{}, :unchoke) when is_map(p) do
    {
      Map.put(p, :peer_choking, false),
      []
    }
  end

  def recv(p = %__MODULE__{}, {:have, i}) when is_map(p) do
    {
      Map.update!(p, :has, &IntSet.put(&1, i)),
      []
    }
  end

  def recv(p = %__MODULE__{}, _) when is_map(p) do
    {p, []}
  end
end
