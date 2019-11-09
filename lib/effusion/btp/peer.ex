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
  defguard is_peer_id(term) when not is_nil(term) and is_binary(term) and byte_size(term) == 20

  defstruct [
    :address,
    :peer_id,
    failcount: 0,
    # peer is choking this client
    peer_choking: true,
    # peer is interested in this client
    peer_interested: false,
    # this client is choking the peer
    am_choking: true,
    # this client is interested in the peer
    am_interested: false,
    has: IntSet.new(),
    blocks_we_requested: MapSet.new()
  ]

  @doc """
  Create a new peer data structure.
  """
  def new({_host, port} = address, peer_id \\ nil) when port > 0 and (is_peer_id(peer_id) or is_nil(peer_id)) do
    %__MODULE__{address: address, peer_id: peer_id}
  end


  def inc_fail_count(peer = %__MODULE__{}) when is_map(peer) do
    Map.update(peer, :failcount, 0, &(&1 + 1))
  end

  def dec_fail_count(peer = %__MODULE__{}) when is_map(peer) do
    Map.update(peer, :failcount, 0, &(&1 - 1))
  end

  def blocks_we_requested(peer) do
    peer.blocks_we_requested
  end

  def request_block(peer, block_id) do
    Map.update!(peer, :blocks_we_requested, &MapSet.put(&1, block_id))
  end

  def remove_requested_block(peer, %{index: id_i, offset: id_o, size: id_s}) do
    requested =
      peer.blocks_we_requested
      |> Enum.reject(fn %{index: i, offset: o, size: s} ->
        i == id_i && o == id_o && s == id_s
      end)
      |> MapSet.new()

    %{peer | blocks_we_requested: requested}
  end

  def availability_map(peer) do
    peer.has
    |> Enum.reduce(Map.new(), fn p, map ->
      Map.update(map, p, [peer.peer_id], &[peer.peer_id | &1])
    end)
  end

  def drop_requests(peer) do
    %{peer | blocks_we_requested: MapSet.new()}
  end

  def unchoke(peer) do
    {
      Map.put(peer, :am_choking, false),
      [:unchoke]
    }
  end

  def interested(peer) do
    {
      Map.put(peer, :am_interested, true),
      [:interested]
    }
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message, and return the updated peer
  along with the messages to send back to the remote peer.
  """
  def recv(peer, message)

  def recv(p = %__MODULE__{}, {:bitfield, b}) when is_map(p) do
    {Map.put(p, :has, IntSet.new(b)), []}
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
