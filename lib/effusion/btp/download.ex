defmodule Effusion.BTP.Download do
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.AvailabilityMap
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Block
  alias Effusion.BTP.Request
  alias Effusion.BTP.Swarm
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo
  alias Effusion.Repo
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  import Effusion.Math
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
    :swarm,
    :availability_map,
    :block_size,
    :torrent,
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
    info_hash = meta.info_hash

    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)
    torrent = if is_nil(torrent) do
      {:ok, torrent} = %Torrent{
        info_hash: meta.info_hash,
        name: meta.info.name
      } |> Torrent.changeset()
      |> Repo.insert()

      IntSet.new()
      |> IntSet.inverse(meta.info.pieces |> Enum.count)
      |> Enum.each(fn index ->
        {:ok, piece} =
          %Piece{
            torrent: torrent,
            index: index,
            hash: Enum.at(meta.info.pieces, index),
            size: piece_size(index, meta.info),
            blocks: []
          }
          |> Repo.insert()

        block_entries = Enum.map(Block.split(piece, @block_size), fn block ->
            %{
              piece_id: piece.id,
              offset: block.offset,
              size: block.size
            }
        end)
        Repo.insert_all(Block, block_entries)
      end)

      torrent
    else
      torrent
    end

    %__MODULE__{
      file: file,
      meta: meta,
      torrent: torrent,
      peer_id: @local_peer_id,
      pieces: Pieces.new(meta.info_hash),
      local_address: local_address,
      swarm: Swarm.new(@local_peer_id, meta.info_hash),
      availability_map: AvailabilityMap.new(),
      block_size: @block_size,
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

    swarm = Swarm.add(d.swarm, res.peers)

    d =
      d
      |> Map.put(:trackerid, trackerid)
      |> Map.put(:swarm, swarm)

    max_peers = Application.get_env(:effusion, :max_peers)
    eligible_peers = PeerSelection.select_lowest_failcount([], max_peers)

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

    peer = Swarm.select_peer(d.swarm, from) |> Repo.preload(:blocks_we_requested)

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
    pieces_query = from p in Piece,
                    join: torrent in assoc(p, :torrent),
                    where: torrent.info_hash == ^d.meta.info_hash
                     and p.index in ^indicies

    pieces = Repo.all(pieces_query)
    Enum.each(pieces, fn p ->
      Repo.insert(%PeerPiece{
        peer: peer,
        piece: p
      })
    end)

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
    Map.update!(d, :swarm, &Swarm.handle_connect(&1, peer_id, address))
  end

  def next_request_from_peer(d, peer_id, count \\ 1) do
    info_hash = d.meta.info_hash
    existing_requests = from requests in Request,
                          join: peer in assoc(requests, :peer),
                          join: block in assoc(requests, :block),
                          join: piece in assoc(block, :piece),
                          join: torrent in assoc(piece, :torrent),
                          where: torrent.info_hash == ^info_hash,
                          select: {piece, block, peer}

    requests_to_make = from peer_pieces in PeerPiece,
                        join: piece in assoc(peer_pieces, :piece),
                        join: block in assoc(piece, :blocks),
                        join: peer in assoc(peer_pieces, :peer),
                        join: torrent in assoc(piece, :torrent),
                        where: torrent.info_hash == ^d.meta.info_hash and peer.peer_id == ^peer_id,
                        except: ^existing_requests,
                        limit: ^count,
                        select: {piece, block, peer}

    requests = Repo.all(requests_to_make)

    requests |> Enum.reduce({d, []}, fn {piece, block, peer}, {d, requests} ->
      req = {:btp_send, peer.peer_id, {:request, piece.index, block.offset, block.size}}

      {:ok, _block} = Repo.insert(%Effusion.BTP.Request{
        block: block,
        peer: peer
      })
      {d, [req | requests]}
    end)
  end

  defp piece_size(index, info) do
    {whole_piece_count, last_piece_size} = divrem(info.length, info.piece_length)
    last_piece_index = whole_piece_count

    if index == last_piece_index do
      last_piece_size
    else
      info.piece_length
    end
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
      peer -> {swarm, [connect_message(
                        peer.address.address,
                        peer.port,
                        swarm.info_hash,
                        swarm.peer_id,
                        peer.peer_id)]}
    end
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
