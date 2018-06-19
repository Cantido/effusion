defmodule Effusion.BTP.Peer do
  @moduledoc """
  Functions for manipulating a member of a BitTorrent swarm.
  """

  @doc """
  Create a new peer data structure.

  Note that the `peer_id` argument is our *local* peer ID.
  To set the ID of the remote peer, see `set_remote_peer_id/2`.
  """
  def new({_host, _port} = address, peer_id, info_hash, session) when is_pid(session) do
    %{
      address: address,
      peer_id: peer_id,
      remote_peer_id: nil,
      info_hash: info_hash,
      session: session,
      handshaken: false,
      peer_choking: true,
      peer_interested: false,
      am_choking: true,
      am_interested: false,
      has: IntSet.new()
    }
  end

  @doc """
  Set the 20-byte peer ID value that identifies the remote peer.
  """
  def set_remote_peer_id(p = %{peer_id: peer_id}, remote_peer_id) when peer_id != remote_peer_id do
    Map.put(p, :remote_peer_id, remote_peer_id)
  end

  @doc """
  Get the handshake message that this peer would send to its remote.
  """
  def get_handshake(p) do
    {:handshake, p.peer_id, p.info_hash}
  end

  @doc """
  Accept a handshake message received by a remote peer, and validate it.
  """
  def handshake(p, {:handshake, remote_peer_id, info_hash, _reserved}) do
    cond do
      p.handshaken ->
        {:error, :local_peer_already_handshaken}
      p.info_hash != info_hash ->
        {:error, :mismatched_info_hash, [expected: p.info_hash, actual: info_hash]}
      true ->
        {:ok, %{p | handshaken: true, remote_peer_id: remote_peer_id}}
    end
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message, and return the updated peer
  along with the messages to send back to the remote peer.
  """
  def recv(peer, message)

  def recv(p, {:bitfield, b}) do
    p = p
      |> Map.put(:has, IntSet.new(b))
      |> Map.put(:am_choking, false)
      |> Map.put(:am_interested, true)

    {p, [:interested, :unchoke]}
  end

  def recv(p, :unchoke) do
    {
      Map.put(p, :peer_choking, false),
      []
    }
  end

  def recv(p, {:have, i}) do
    {
      Map.update!(p, :has, &IntSet.put(&1, i)),
      []
    }
  end

  def recv(p, _) do
    {p, []}
  end
end
