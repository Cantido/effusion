defmodule Effusion.BTP.Download do
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.PiecePicker
  alias Effusion.BTP.Block
  alias Effusion.BTP.Swarm
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  require Logger
  use Timex

  @moduledoc """
  The top-level data structure of a downloading file.

  This module decides when to connect to peers, when to ask for pieces,
  when to write pieces to disk, etc.
  """

  defstruct [
    :file,
    :meta,
    :peer_id,
    :pieces,
    :local_address,
    :swarm,
    started_at: nil,
    listeners: MapSet.new,
    tracker_id: ""
  ]

  @local_peer_id Application.get_env(:effusion, :peer_id)

  @block_size Application.get_env(:effusion, :block_size)

  @doc """
  Create a new download.

  This only creates a data structure. To actually start the download, call `start/1`.
  """
  def new(meta, local_address, file \\ nil) do
    %__MODULE__{
      file: file,
      meta: meta,
      peer_id: @local_peer_id,
      pieces: Pieces.new(meta.info_hash),
      local_address: local_address,
      swarm: Effusion.BTP.Swarm.new(@local_peer_id, meta.info_hash)
    }
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session = %__MODULE__{}) do
    _ = Logger.info "Starting download #{Effusion.Hash.inspect session.meta.info_hash}"
    session = Map.put(session, :started_at, Timex.now())
    params = announce_params(session, :started)

    messages = [{:announce, params}]
    {session, messages}
  end

  @doc """
  Get the blocks that have not been assembled into pieces and verified.
  """
  def blocks(d = %__MODULE__{}) do
    Pieces.unfinished(d.pieces)
  end

  @doc """
  Get the set of listeners waiting for this torrent to finish.
  """
  def listeners(d = %__MODULE__{}) do
    d.listeners
  end

  @doc """
  Get the pieces that this download is downloading.
  """
  def pieces(d = %__MODULE__{}) do
    d.pieces
  end

  def mark_piece_written(d = %__MODULE__{}, i) do
    Map.update(d, :pieces, Pieces.new(d.meta.info_hash), &Pieces.mark_piece_written(&1, i))
  end

  @doc """
  Add a process that should be notified when this download completes or crashes.
  """
  def add_listener(d = %__MODULE__{}, from) do
    Map.update(d, :listeners, MapSet.new(), &MapSet.put(&1, from))
  end

  @doc """
  Perform a function on all of this download's listening processes.
  """
  def each_listener(%__MODULE__{listeners: listeners}, fun)
      when is_function(fun, 1) do
    Enum.each(listeners, &fun.(&1))
  end

  @doc """
  Check if this download has received all necessary bytes.
  """
  def done?(d = %__MODULE__{}) do
    Pieces.all_present?(d.pieces)
  end

  def announce_params(d, event) do
    {local_host, local_port} = d.local_address

    [
      d.meta.announce,
      local_host,
      local_port,
      d.peer_id,
      d.meta.info_hash,
      0,
      Pieces.bytes_completed(d.pieces),
      Pieces.bytes_left(d.pieces),
      event,
      d.tracker_id
    ]
  end

  def handle_tracker_response(d, res) do
    tracker_id =
      if Map.get(res, :tracker_id, "") != "" do
        res.tracker_id
      else
        d.tracker_id
      end

    swarm = Swarm.add(d.swarm, res.peers)

    d = d
    |> Map.put(:tracker_id, tracker_id)
    |> Map.put(:swarm, swarm)

    eligible_peers = PeerSelection.select_lowest_failcount(Swarm.peers(swarm), 5)

    messages = Enum.map(eligible_peers, fn p -> {:btp_connect, p} end)
    {d, messages}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %__MODULE__{peer_id: peer_id}, remote_peer_id, msg)
      when is_peer_id(peer_id)
       and is_peer_id(remote_peer_id)
       and peer_id != remote_peer_id do
    with {d, peer_messages} = delegate_message(d, remote_peer_id, msg),
         {:ok, d, session_messages} <- session_handle_message(d, remote_peer_id, msg) do
      {d, session_messages ++ peer_messages}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Add a block of data to this download.

  This may trigger messages to be sent to any connections associated with this download's torrent.
  """
  def add_block(d = %__MODULE__{}, block, from) when is_peer_id(from) do
    {swarm, cancel_messages} = Swarm.cancel_block_requests(d.swarm, block, from)
    d = %{d | swarm: swarm}

    pieces = Pieces.add_block(d.pieces, block)
    verified = Pieces.verified(pieces)

    have_messages = verified
    |> Enum.map(&({:broadcast, {:have, &1.index}}))

    write_messages = pieces
    |> Pieces.verified()
    |> Enum.map(fn p -> {:write_piece, pieces.info, d.file, p} end)

    {
      %{d | pieces: pieces},
      write_messages ++ have_messages ++ cancel_messages
    }
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:piece, b}) when is_peer_id(remote_peer_id) do
    {d, block_messages} = add_block(d, b, remote_peer_id)

    # request more pieces from that peer
    peer = Swarm.select_peer(d.swarm, remote_peer_id)
    {d, request_messages} = next_requests(d, [peer])

    {:ok, d, block_messages ++ request_messages}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:bitfield, b}) do
    interest_message = if need_any_pieces?(d, b) do
      [{:btp_send, remote_peer_id, :interested}]
    else
      []
    end
    {:ok, d, interest_message}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:have, b}) do
    interest_message = if need_piece?(d, b) do
      [{:btp_send, remote_peer_id, :interested}]
    else
      []
    end
    {:ok, d, interest_message}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, :unchoke) do
    peer = Swarm.select_peer(d.swarm, remote_peer_id)
    {d, request_messages} = next_requests(d, [peer])
    {:ok, d, request_messages}
  end

  defp session_handle_message(d = %__MODULE__{}, _remote_peer_id, _msg), do: {:ok, d, []}

  defp delegate_message(d, remote_peer_id, msg) when is_peer_id(remote_peer_id) do
    {swarm, messages} = Swarm.delegate_message(d.swarm, remote_peer_id, msg)
    {%{d | swarm: swarm}, messages}
  end

  def handle_connect(d, peer_id, address) when is_peer_id(peer_id) do
    Map.update!(d, :swarm, &Swarm.handle_connect(&1, peer_id, address))
  end

  def need_piece?(d, i) do
    !Pieces.has_piece?(d.pieces, i)
  end

  def need_any_pieces?(d, bitfield) do
    !Pieces.has_pieces?(d.pieces, bitfield)
  end

  def next_requests(d = %__MODULE__{}, peers) do
    next_blocks =
      Effusion.BTP.PiecePicker.next_blocks(
        d.pieces,
        peers,
        @block_size
      )

    case next_blocks do
      [] -> {d, []}
      _ ->
        d = Map.update!(d, :swarm, &Swarm.mark_blocks_requested(&1, next_blocks))
        next_requests = next_blocks |> Enum.map(&block_into_request/1)
        {d, next_requests}
    end
  end

  defp block_into_request({peer_id, %{index: i, offset: o, size: sz}}) do
    {:btp_send, peer_id, {:request, i, o, sz}}
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(d = %__MODULE__{}, address, reason \\ :normal) do
    {swarm, messages} = d.swarm
    |> Swarm.handle_disconnect(address, reason)
    |> increment_connections()

    {
      %{d | swarm: swarm},
      messages
    }
  end

  defp increment_connections(swarm) do
    [selected] = PeerSelection.select_lowest_failcount(Swarm.peers(swarm), 1)

    case selected do
      nil -> {swarm, []}
      peer -> {swarm, [{:btp_connect, peer}]}
    end
  end
end
