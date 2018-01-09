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

  def choke(state) do
    {:ok, %{state | choked: true}}
  end

  def unchoke(state) do
    {:ok, %{state | choked: false}}
  end

  def interested(state) do
    {:ok, %{state | interested: true}}
  end

  def uninterested(state) do
    {:ok, %{state | interested: false}}
  end

  def have(i, state) do
    {:ok, %{state | have: Effusion.IntSet.put(state.have, i) }}
  end

  def bitfield(b, state) do
    # BUG: The BitTorrent spec states that the high bit of the first byte
    # corresponds to index 0, so this binary needs to be reversed
    # in order to be used with IntSet
    have_pcs = Effusion.IntSet.new(b)
    {:ok, %{state | have: Effusion.IntSet.union(state.have, have_pcs) }}
  end

  def request(block, state) do
    next_requested = MapSet.put(state.requested, block)
    {:ok, %{state | requested: next_requested}}
  end

  def piece(block, state) do
    next_blocks = MapSet.put(state.blocks, block)
    {:ok, %{state | blocks: next_blocks}}
  end

  def cancel(block, state) do
    next_requested = MapSet.delete(state.requested, block)
    {:ok, %{state | requested: next_requested}}
  end

  def handle_msg({msg_type}, state) do
    case msg_type do
      :choke -> choke(state)
      :unchoke -> unchoke(state)
      :interested -> interested(state)
      :uninterested -> uninterested(state)
    end
  end

  def handle_msg({msg_type, payload}, state) do
    case msg_type do
      :have -> have(payload, state)
      :bitfield -> bitfield(payload, state)
      :request -> request(payload, state)
      :piece -> piece(payload, state)
      :cancel -> cancel(payload, state)
    end
  end
end
