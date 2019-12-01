defmodule Effusion.BTP.DownloadServer do
  use GenServer, restart: :transient
  alias Effusion.Application.DownloadServerSupervisor
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Piece
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo.Directory
  alias Effusion.BTP.VerifierWatchdog
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.OutgoingHandler
  alias Effusion.THP.Announcer
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  An API to manage a `Effusion.BTP.Download` object as it is connected to many peers simultaneously.
  """

  @local_peer_id Application.get_env(:effusion, :peer_id)

  ## API

  @doc """
  Start the Download Server in its own supervision tree.
  """
  def start(meta) when is_map(meta) do
    case DownloadServerSupervisor.start_child([meta]) do
      {:ok, _pid} -> {:ok, meta.info_hash}
      err -> err
    end
  end

  @doc """
  Start the session server and link it to the current process.
  """
  def start_link([meta]) do
    GenServer.start_link(
      __MODULE__,
      meta,
      name: {:via, Registry, {SessionRegistry, meta.info_hash}}
    )
  end

  def get(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :get, 10_000)
  end

  def notify_all_pieces_written(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :all_pieces_written)
  end

  @doc """
  Wait on a download managed by a session server to complete.
  """
  def await(info_hash) do
    GenServer.call({:via, Registry, {SessionRegistry, info_hash}}, :await, :infinity)
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(info_hash, from, {:piece, block}) when is_hash(info_hash) and is_peer_id(from) do
    Request.cancel(block, from)
    |> Enum.uniq()
    |> Enum.each(fn {peer_id, index, offset, size} ->
      ConnectionRegistry.btp_send(info_hash, peer_id, {:cancel, index, offset, size})
    end)

    Pieces.add_block(info_hash, block)

    peer_request_query = from peer_piece in PeerPiece,
                         join: peer in assoc(peer_piece, :peer),
                         where: peer.peer_id == ^from
    peer_request_count = Repo.aggregate(peer_request_query, :count, :peer_id)

    if peer_request_count <= 0 do
      next_request_from_peer(info_hash, from, Application.get_env(:effusion, :max_requests_per_peer))
    end
    :ok
  end

  def handle_message(info_hash, remote_peer_id, {:bitfield, bitfield}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer = Repo.one!(from p in Peer, where: [peer_id: ^remote_peer_id])
    indicies = IntSet.new(bitfield) |> Enum.to_list()
    pieces_query = Piece.all_indicies_query(info_hash, indicies)
    pieces = Repo.all(pieces_query)
    peer_pieces = Enum.map(pieces, fn p ->
      %{
        peer_id: peer.id,
        piece_id: p.id
      }
    end)

    Repo.insert_all(PeerPiece, peer_pieces)

    if !Pieces.has_pieces?(info_hash, bitfield) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end

    :ok
  end

  def handle_message(info_hash, remote_peer_id, {:have, i}) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    peer = from p in Peer, where: p.peer_id == ^remote_peer_id
    piece = from p in Piece,
             join: torrent in assoc(p, :torrent),
             where: torrent.info_hash == ^info_hash
                and p.index == ^i

    peer = Repo.one!(peer)
    piece = Repo.one!(piece)

    Repo.insert(%PeerPiece{
      peer: peer,
      piece: piece
    })

    if !Pieces.has_piece?(info_hash, i) do
      ConnectionRegistry.btp_send(info_hash, remote_peer_id, :interested)
    end
    :ok
  end

  def handle_message(info_hash, remote_peer_id, :choke) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    Repo.delete_all(from request in Request,
                      join: peer in assoc(request, :peer),
                      where: peer.peer_id == ^remote_peer_id)

    :ok
  end

  def handle_message(info_hash, remote_peer_id, message) when is_hash(info_hash) and is_peer_id(remote_peer_id) do
    next_request_from_peer(info_hash, remote_peer_id, 100)
    :ok
  end

  @doc """
  Handle a peer disconnection.
  """
  def unregister_connection(info_hash, {ip, port}, reason) do
    PeerSelection.select_lowest_failcount(1)
        |> Enum.map(fn peer ->
          address = {peer.address.address, peer.port}
          OutgoingHandler.connect({address, info_hash, @local_peer_id, peer.peer_id})
        end)

    Repo.one(from peer in Peer,
              where: peer.address == ^%Postgrex.INET{address: ip}
              and peer.port == ^port)
    |> Peer.changeset()
    |> Repo.update(update: [inc: [failcount: 1]])

    :ok
  end

  def connected(info_hash, peer_id, address) when is_hash(info_hash) and is_peer_id(peer_id) do
    Peer.insert(peer_id, address)
    :ok
  end

  defp next_request_from_peer(info_hash, peer_id, count) when is_hash(info_hash) do
    requests = Request.valid_requests_from_peer_query(info_hash, peer_id, count)
    |> Repo.all()

    requests_to_insert = Enum.map(requests, fn {_piece, block, peer} ->
      %{
        block_id: block.id,
        peer_id: peer.id
      }
    end)
    Repo.insert_all(Request, requests_to_insert)

    Enum.each(requests, fn {piece, block, peer} ->
      ConnectionRegistry.btp_send(info_hash, peer.peer_id, {:request, piece.index, block.offset, block.size})
    end)
    :ok
  end

  ## Callbacks

  def init(meta) do
    :ok = Directory.insert(meta)
    info_hash = meta.info_hash

    torrent = Repo.one(from t in Torrent, where: t.info_hash == ^info_hash)
    if is_nil(torrent) do
      {:ok, _torrent} = Torrent.insert(meta)
    end

    state = %{
      info_hash: meta.info_hash,
      meta: meta,
      peer_id: @local_peer_id,
      listeners: MapSet.new()
    }

    {:ok, state, 0}
  end

  def handle_call(:all_pieces_written, _from, d) do
    :ok = Announcer.announce(d.info_hash, :completed)

    reply_to_listeners(d, :ok)

    {:stop, :normal, :ok, d}
  end

  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:await, from, state) do
    state = Map.update(state, :listeners, MapSet.new(), &MapSet.put(&1, from))
    {:noreply, state}
  end

  def handle_info(:timeout, session) do
    _ = Logger.info("Starting download #{Effusion.Hash.inspect(session.info_hash)}")

    Repo.delete_all(PeerPiece)
    Repo.delete_all(Request)

    VerifierWatchdog.start(session.info_hash)

    session = Map.put(session, :started_at, Timex.now())
    Repo.one!(from torrent in Torrent,
              where: torrent.info_hash == ^session.info_hash)
    |> Torrent.start(Timex.now())
    |> Repo.update()

    :ok = Announcer.announce(session.info_hash, :started)

    {:noreply, session}
  end

  def handle_info(:interval_expired, state) do
    :ok = Announcer.announce(state.info_hash, :interval)

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(:normal, state) do
    ConnectionRegistry.disconnect_all(state.meta.info_hash)

      if Pieces.all_written?(state.pieces) do
        :ok = Announcer.announce(state.info_hash, :completed)
      else
        :ok = Announcer.announce(state.info_hash, :stopped)
      end

    reply_to_listeners(state, :ok)
  end

  def terminate(reason, state) do
    Logger.debug("download server terminating with reason: #{inspect(reason)}")
    ConnectionRegistry.disconnect_all(state.meta.info_hash)

    :ok = Announcer.announce(state.info_hash, :stopped)

    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state, msg) do
    Enum.each(state.listeners, fn l -> GenServer.reply(l, msg) end)
  end
end
