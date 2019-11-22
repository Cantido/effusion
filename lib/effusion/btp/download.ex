defmodule Effusion.BTP.Download do
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.PiecePicker
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Swarm
  alias Effusion.BTP.Metainfo
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
    listeners: MapSet.new(),
    trackerid: ""
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
      swarm: Swarm.new(@local_peer_id, meta.info_hash)
    }
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session = %__MODULE__{}) do
    _ = Logger.info("Starting download #{Effusion.Hash.inspect(session.meta.info_hash)}")
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

  def bytes_completed(download) do
    Pieces.bytes_completed(download.pieces)
  end

  def download_size(download) do
    Metainfo.bytes_count(download.meta)
  end

  def downloaded_ratio(download) do
    {
      bytes_completed(download),
      download_size(download)
    }
  end

  def download_duration(download) do
    Timex.Interval.new(from: download.started_at, until: Timex.now())
    |> Timex.Interval.duration(:duration)
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

    tracker_numwant = Application.get_env(:effusion, :tracker_numwant)
    opts = [event: event, numwant: tracker_numwant]

    opts = case d.trackerid do
      "" -> opts
      str -> opts |> Keyword.merge([trackerid: d.trackerid])
    end

    [
      d.meta.announce,
      local_host,
      local_port,
      d.peer_id,
      d.meta.info_hash,
      0,
      Pieces.bytes_completed(d.pieces),
      Pieces.bytes_left(d.pieces),
      opts
    ]
  end

  def handle_tracker_response(d, res) do
    trackerid =
      if Map.get(res, :trackerid, "") != "" do
        res.trackerid
      else
        d.trackerid
      end

    swarm = Swarm.add(d.swarm, res.peers)

    d =
      d
      |> Map.put(:trackerid, trackerid)
      |> Map.put(:swarm, swarm)

    max_peers = Application.get_env(:effusion, :max_peers)
    eligible_peers = PeerSelection.select_lowest_failcount(Swarm.peers(swarm), max_peers)

    messages = Enum.map(eligible_peers, fn p ->
      {
        :btp_connect,
        p.address,
        d.meta.info_hash,
        d.peer_id,
        p.peer_id
      }
    end)
    {d, messages}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %__MODULE__{peer_id: peer_id}, remote_peer_id, msg)
      when is_peer_id(peer_id) and
             is_peer_id(remote_peer_id) and
             peer_id != remote_peer_id do
    case session_handle_message(d, remote_peer_id, msg) do
      {:ok, d, session_messages} ->
        {d, peer_messages} = delegate_message(d, remote_peer_id, msg)
        {d, session_messages ++ peer_messages}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp session_handle_message(d = %__MODULE__{}, from, {:piece, block})
       when is_peer_id(from) do
     {swarm, cancel_messages} = Swarm.cancel_block_requests(d.swarm, block, from)
     d = %{d | swarm: swarm}

     d = Map.update!(d, :pieces, &Pieces.add_block(&1, block))
     verified = Pieces.verified(d.pieces)

     have_messages =
       verified
       |> Enum.map(&{:broadcast, {:have, &1.index}})

     write_messages =
       d.pieces
       |> Pieces.verified()
       |> Enum.map(fn p -> {:write_piece, d.meta.info, d.file, p} end)

    peer = Swarm.select_peer(d.swarm, from)
    {d, request_messages} = next_requests(d, [peer])

    {:ok, d, write_messages ++ have_messages ++ cancel_messages ++ request_messages}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:bitfield, bitfield}) do
    interest_message =
      if Pieces.has_pieces?(d.pieces, bitfield) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {:ok, d, interest_message}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, {:have, i}) do
    interest_message =
      if Pieces.has_piece?(d.pieces, i) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {:ok, d, interest_message}
  end

  defp session_handle_message(d = %__MODULE__{}, remote_peer_id, :choke) do
    d = Map.update!(d, :swarm, &Swarm.drop_requests(&1, remote_peer_id))
    {:ok, d, []}
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

  defp next_requests(d = %__MODULE__{}, peers) do
    next_blocks =
      PiecePicker.next_blocks(
        d.pieces,
        peers,
        @block_size
      )

    case next_blocks do
      [] ->
        {d, []}

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
    {swarm, messages} =
      d.swarm
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
      peer -> {swarm, [{
                        :btp_connect,
                        peer.address,
                        swarm.info_hash,
                        swarm.peer_id,
                        peer.peer_id
                      }]}
    end
  end
end
