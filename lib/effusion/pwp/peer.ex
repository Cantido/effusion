defmodule Effusion.PWP.Peer do
  require Logger
  alias Effusion.LocalPeer
  @behaviour Effusion.PWP.GenPeer

  def init do
    %{
      interested: false,
      choked: true,
      have: Effusion.IntSet.new(),
      blocks: MapSet.new(),
      requested: MapSet.new()
    }
  end

  def handle_handshake({peer_id, info_hash, _reserved}, _state) do
    with :ok <- check_info_hash(info_hash),
         :ok <- check_peer_id(peer_id),
         local_peer_id <- LocalPeer.peer_id()
    do
      :ok = Logger.info ("Handshake from peer_id #{Base.encode16(peer_id)} for info_hash #{Base.encode16(info_hash)}")
      {:ok,
        {local_peer_id, info_hash, <<0 :: 64>>},
        %{remote_peer_id: peer_id}}
    else
      err -> err
    end
  end

  @spec check_peer_id(Effusion.InfoHash.t) :: :ok | {:error, :unknown_info_hash}
  defp check_info_hash(info_hash) do
    case Registry.lookup(Effusion.TorrentRegistry, info_hash) do
      [{_pid, :ok}] -> :ok
      [] -> {:error, :unknown_info_hash}
    end
  end

  @spec check_peer_id(Effusion.PeerId.t) :: :ok | {:error, :remote_same_as_local}
  defp check_peer_id(remote_peer_id) do
    if LocalPeer.matches_id?(remote_peer_id) do
      {:error, :remote_same_as_local}
    else
      :ok
    end
  end

  def handle_msg({:choke}, state) do
    {:ok, %{state | choked: true}}
  end

  def handle_msg({:unchoke}, state) do
    {:ok, %{state | choked: false}}
  end

  def handle_msg({:interested}, state) do
    {:ok, %{state | interested: true}}
  end

  def handle_msg({:uninterested}, state) do
    {:ok, %{state | interested: false}}
  end

  def handle_msg({:have, idx}, state) do
    # TODO: A peer receiving this message MUST send an interested message to
    # the sender if indeed it lacks the piece announced.
    # TODO: A peer receiving this message MAY send a request for that piece.
    {:ok, %{state | have: Effusion.IntSet.put(state.have, idx) }}
    end

  def handle_msg({:bitfield, bitfield}, state) do

    # A peer MUST send this message immediately after the handshake operation,
    # and MAY choose not to send it if it has no pieces at all.

    # This message MUST not be sent at any other time during the communication.

    {:ok, %{state | have: Effusion.IntSet.new(bitfield) }}
  end

  def handle_msg({:request, block}, state) do

    # TODO The recipient MUST only send piece messages to a sender that has
    # already requested it, and only in accordance to the rules
    # about the choke and interested states.

    next_requested = MapSet.put(state.requested, block)
    {:ok, %{state | requested: next_requested}}
  end

  def handle_msg({:piece, block}, state) do
    next_blocks = MapSet.put(state.blocks, block)
    {:ok, %{state | blocks: next_blocks}}
  end

  def handle_msg({:cancel, block}, state) do
    next_requested = MapSet.delete(state.requested, block)
    {:ok, %{state | requested: next_requested}}
  end
end
