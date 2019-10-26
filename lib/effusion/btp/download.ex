defmodule Effusion.BTP.Download do
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Block
  alias Effusion.BTP.Swarm
  import Effusion.BTP.Peer
  require Logger

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
    listeners: MapSet.new,
    requested_pieces: MapSet.new(),
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

  @doc """
  Add a block of data to this download.

  This may trigger messages to be sent to any connections associated with this download's torrent.
  """
  def add_block(d = %__MODULE__{}, block, from) when is_peer_id(from) do
    {d, cancel_messages} = cancel_block_requests(d, block, from)

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

  def mark_piece_written(d = %__MODULE__{}, i) do
    Map.update(d, :pieces, Pieces.new(d.meta.info_hash), &Pieces.mark_piece_written(&1, i))
  end

  defp cancel_block_requests(d, block, from) do
    block_id = Block.id(block)

    messages =
      d
      |> Map.get(:requested_pieces, MapSet.new())
      |> Map.get(block_id, MapSet.new())
      |> Enum.filter(&(&1 != from))
      |> Enum.map(&({:btp_send, &1, {:cancel, block_id}}))

    d = Map.update(d, :requested_pieces, Map.new(), &Map.delete(&1, block_id))

    {d, messages}
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

  @doc """
  Get the next piece that this download should ask for.
  """
  def next_request(d = %__MODULE__{}) do
    next_block =
      Effusion.BTP.PieceSelection.next_block(
        d.pieces,
        Map.values(d.swarm.peers),
        # actual_connected_peers,
        @block_size
      )

    case next_block do
      nil -> {nil, d}
      _ ->
        s1 = Map.update(d, :requested_pieces, MapSet.new(), &MapSet.put(&1, next_block))
        {next_block, s1}
    end
  end

  defp next_request_msg(session = %__MODULE__{}) do
    case next_request(session) do
      {{peer_id, %{index: i, offset: o, size: sz}}, session} -> {session, [{:btp_send, peer_id, {:request, i, o, sz}}]}
      {nil, session} -> {session, []}
    end
  end

  @doc """
  Announce an event to this download's tracker,
  using the given Tracker HTTP Protocol (THP) client.
  """
  def announce(d = %__MODULE__{}, client, event \\ :interval) do
    {local_host, local_port} = Map.fetch!(d, :local_address)

    {:ok, res} =
      client.announce(
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
      )

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

    {d, res}
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session = %__MODULE__{}, thp_client) do
    _ = Logger.info "Starting download #{Effusion.Hash.inspect session.meta.info_hash}"
    {session, response} = announce(session, thp_client, :started)
    messages = Enum.map(session.swarm.peers, fn {_addr, p} -> {:btp_connect, p} end)
    {session, response, messages}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %__MODULE__{peer_id: peer_id}, remote_peer_id, msg)
      when is_peer_id(peer_id) and is_peer_id(peer_id) and peer_id != remote_peer_id do
    with {:ok, d, session_messages} <- session_handle_message(d, remote_peer_id, msg) do
      {d, peer_messages} = delegate_message(d, remote_peer_id, msg)
      {d, session_messages ++ peer_messages}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp session_handle_message(d = %__MODULE__{}, _remote_peer_id, {:bitfield, b}) do
    pieces_count = Enum.count(d.meta.info.pieces)
    max_i = Enum.max(IntSet.new(b), fn -> 0 end)

    if max_i in 0..(pieces_count - 1) do
      {:ok, d, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(d = %__MODULE__{}, _remote_peer_id, {:have, i}) do
    pieces_count = Enum.count(d.meta.info.pieces)

    if i in 0..(pieces_count - 1) do
      {:ok, d, []}
    else
      {:error, :index_out_of_bounds}
    end
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:piece, b}) do
    {d, block_messages} = add_block(d, b, remote_peer_id)
    {d, request_messages} = next_request_msg(d)
    {:ok, d, block_messages ++ request_messages}
  end

  defp session_handle_message(d = %__MODULE__{}, _remote_peer_id, :unchoke) do
    {d, req} = next_request_msg(d)
    {:ok, d, req}
  end

  defp session_handle_message(d = %__MODULE__{}, _remote_peer_id, _msg), do: {:ok, d, []}

  defp delegate_message(d, remote_peer_id, msg)
       when is_peer_id(remote_peer_id) do

    {swarm, messages} = Swarm.delegate_message(d.swarm, remote_peer_id, msg)
    {
      %{d | swarm: swarm},
      messages
    }
  end

  def handle_connect(d, peer_id, address) when is_peer_id(peer_id) do
    Map.update!(d, :swarm, &Swarm.handle_connect(&1, peer_id, address))
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(d = %__MODULE__{}, peer_id, address)
      when is_peer_id(peer_id) do
    {swarm, messages} = d.swarm
    |> Swarm.handle_disconnect(address)
    |> increment_connections()

    {
      %{d | swarm: swarm},
      messages
    }
  end

  defp increment_connections(swarm) do
    selected = Effusion.BTP.PeerSelection.select_peer(
      swarm.peer_id,
      swarm.peers,
      []
    )

    case selected do
      nil -> {swarm, []}
      peer -> {swarm, [{:btp_connect, peer}]}
    end
  end
end
