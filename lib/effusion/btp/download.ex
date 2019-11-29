defmodule Effusion.BTP.Download do
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.AvailabilityMap
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
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
    started_at: nil,
    listeners: MapSet.new(),
    trackerid: ""
  ]


  @local_peer_id Application.get_env(:effusion, :peer_id)

  @doc """
  Create a new download.

  This only creates a data structure. To actually start the download, call `start/1`.
  """
  def new(meta, local_address, file \\ nil) do
    info_hash = meta.info_hash

    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)
    torrent = if is_nil(torrent) do
      {:ok, torrent} = Torrent.insert(meta)
      torrent
    else
      torrent
    end

    %__MODULE__{
      file: file,
      meta: meta,
      peer_id: @local_peer_id,
      pieces: Pieces.new(meta.info_hash),
      local_address: local_address
    }
  end

  @doc """
  Start a download.

  This means the session will make an announcement to the tracker and begin
  making connections.
  """
  def start(session = %__MODULE__{}) do
    _ = Logger.info("Starting download #{Effusion.Hash.inspect(session.meta.info_hash)}")

    Repo.delete_all(PeerPiece)
    Repo.delete_all(Request)

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
      _str -> opts |> Keyword.merge([trackerid: d.trackerid])
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

    changesets = Enum.map(res.peers, fn peer ->
      %{
        address: %Postgrex.INET{address: peer.ip},
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)

    Repo.insert_all(Peer, changesets, on_conflict: {:replace, [:address]},
                   conflict_target: [:peer_id])

    d =
      d
      |> Map.put(:trackerid, trackerid)

    max_peers = Application.get_env(:effusion, :max_peers)
    eligible_peers = PeerSelection.select_lowest_failcount(max_peers)

    messages = Enum.map(eligible_peers, fn p ->
      connect_message(p.address.address, p.port, d.meta.info_hash, d.peer_id, p.peer_id)
    end)
    {d, messages}
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message send by the remote peer identified by `peer_id`.

  For more information about messages, see `Effusion.PWP.Messages`.
  """
  def handle_message(session, peer_id, message)

  def handle_message(d = %__MODULE__{}, from, {:piece, block})
       when is_peer_id(from) do

     cancel_messages = Request.cancel(block, from)
       |> Enum.map(fn {peer_id, index, offset, size} ->
         {:btp_send, peer_id, {:cancel, index, offset, size}}
       end)
       |> Enum.uniq()


     d = Map.update!(d, :pieces, &Pieces.add_block(&1, block))
     verified = Pieces.verified(d.pieces)

     have_messages =
       verified
       |> Enum.map(&{:broadcast, {:have, &1.index}})

     write_messages =
       d.pieces
       |> Pieces.verified()
       |> Enum.map(fn p -> {:write_piece, d.meta.info, d.file, p} end)

    peer = Repo.one!(from peer in Peer, where: peer.peer_id == ^from) |> Repo.preload(:blocks_we_requested)

    {d, request_messages} = if Enum.empty?(peer.blocks_we_requested) do
      next_request_from_peer(d, from, Application.get_env(:effusion, :max_requests_per_peer))
    else
      {d, []}
    end

    {d, write_messages ++ have_messages ++ cancel_messages ++ request_messages}
  end

  def handle_message(d = %__MODULE__{}, remote_peer_id, {:bitfield, bitfield}) when is_peer_id(remote_peer_id) and is_binary(bitfield) do
    peer = Repo.one!(from p in Peer, where: [peer_id: ^remote_peer_id])
    indicies = IntSet.new(bitfield) |> Enum.to_list()
    pieces_query = Piece.all_indicies_query(d.meta.info_hash, indicies)
    pieces = Repo.all(pieces_query)
    peer_pieces = Enum.map(pieces, fn p ->
      %{
        peer_id: peer.id,
        piece_id: p.id
      }
    end)

    Repo.insert_all(PeerPiece, peer_pieces)

    interest_message =
      if Pieces.has_pieces?(d.pieces, bitfield) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {d, interest_message}
  end

  def handle_message(d = %__MODULE__{}, remote_peer_id, {:have, i}) do
    peer = from p in Peer, where: p.peer_id == ^remote_peer_id
    piece = from p in Piece,
             join: torrent in assoc(p, :torrent),
             where: torrent.info_hash == ^d.meta.info_hash
                and p.index == ^i

    peer = Repo.one!(peer)
    piece = Repo.one!(piece)

    Repo.insert(%PeerPiece{
      peer: peer,
      piece: piece
    })

    interest_message =
      if Pieces.has_piece?(d.pieces, i) do
        []
      else
        [{:btp_send, remote_peer_id, :interested}]
      end

    {d, interest_message}
  end

  def handle_message(d = %__MODULE__{}, remote_peer_id, :choke) do
    Repo.delete_all(from request in Request,
                      join: peer in assoc(request, :peer),
                      where: peer.peer_id == ^remote_peer_id)
    {d, []}
  end

  def handle_message(d = %__MODULE__{}, remote_peer_id, :unchoke) do
    {d, request_messages} = next_request_from_peer(d, remote_peer_id, 100)

    {d, request_messages}
  end

  def handle_message(d = %__MODULE__{}, _remote_peer_id, _msg), do: {:ok, d, []}

  def handle_connect(d, peer_id, address) when is_peer_id(peer_id) do
    Peer.insert(peer_id, address)
    d
  end

  defp next_request_from_peer(d, peer_id, count) do
    info_hash = d.meta.info_hash

    requests = Request.valid_requests_from_peer_query(info_hash, peer_id, count)
    |> Repo.all()

    requests_to_insert = Enum.map(requests, fn {_piece, block, peer} ->
      %{
        block_id: block.id,
        peer_id: peer.id
      }
    end)
    Repo.insert_all(Request, requests_to_insert)

    request_messages = Enum.map(requests, fn {piece, block, peer} ->
      {:btp_send, peer.peer_id, {:request, piece.index, block.offset, block.size}}
    end)

    {d, request_messages}
  end

  @doc """
  Perform actions necessary when a peer at a given address disconnects.
  """
  def handle_disconnect(d = %__MODULE__{}, {ip, port}, _reason \\ :normal) do
    messages = PeerSelection.select_lowest_failcount(1)
        |> Enum.map(fn peer ->
          connect_message(
            peer.address.address,
            peer.port,
            d.meta.info_hash,
            d.peer_id,
            peer.peer_id)
        end)

    Repo.one(from peer in Peer,
              where: peer.address == ^%Postgrex.INET{address: ip}
              and peer.port == ^port)
    |> Peer.changeset()
    |> Repo.update(update: [inc: [failcount: 1]])

    {
      d,
      messages
    }
  end

  defp connect_message(address, port, info_hash, local_peer_id, remote_peer_id)
    when is_hash(info_hash)
     and is_peer_id(local_peer_id)
     and is_peer_id(remote_peer_id) or is_nil(remote_peer_id) do
    {
      :btp_connect,
      {address, port},
      info_hash,
      local_peer_id,
      remote_peer_id
    }
  end
end
