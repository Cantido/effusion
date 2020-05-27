defmodule Effusion.THP.Announcer do
  use GenServer
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.Repo
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  Makes tracker announcements for a torrent, including interval announcements.
  """

  @thp_client Application.get_env(:effusion, :thp_client)

  def start_link(info_hash) do
    GenServer.start_link(
      __MODULE__,
      info_hash,
      name: {:via, Registry, {AnnouncerRegistry, info_hash}}
    )
  end

  @doc """
  Stops the torrent's announcer process
  """
  def stop(info_hash) do
    GenServer.call({:via, Registry, {AnnouncerRegistry, info_hash}}, :stop)
  end

  def init(info_hash) do
    Process.flag(:trap_exit, true)
    {:ok, {info_hash, nil}}
  end

  @doc """
  Announce an event to the torrent's trackers.
  """
  def announce(info_hash, event) when is_hash(info_hash) do
    GenServer.cast({:via, Registry, {AnnouncerRegistry, info_hash}}, {:announce, event})
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_info(:interval_expired, state = {info_hash, _timer}) do
    if Torrent.downloading?(info_hash) do
      handle_cast({:announce, :interval}, state)
    end
  end

  def handle_cast({:announce, event}, {info_hash, timer}) do
    Logger.debug("Announcing #{event}")
    peer_id = Application.get_env(:effusion, :peer_id)
    {local_host, local_port} = Application.get_env(:effusion, :server_address)

    tracker_numwant = Application.get_env(:effusion, :tracker_numwant)
    opts = [event: event, numwant: tracker_numwant]

    {announce, trackerid} = Repo.one!(from torrent in Torrent,
                                      where: torrent.info_hash == ^info_hash,
                                      select: {torrent.announce, torrent.trackerid})

    opts = case trackerid do
      "" -> opts
      _str -> opts |> Keyword.merge([trackerid: trackerid])
    end

    {:ok, res} = @thp_client.announce(
      announce,
      local_host,
      local_port,
      peer_id,
      info_hash,
      0,
      Pieces.bytes_completed(info_hash),
      Pieces.bytes_left(info_hash),
      opts
    )
    if not is_nil(timer) do
      Process.cancel_timer(timer)
    end
    timer = Process.send_after(self(), :interval_expired, res.interval * 1_000)

    torrent = Repo.one!(
      from torrent in Torrent,
      where: torrent.info_hash == ^info_hash
    )
    torrent
    |> Ecto.Changeset.change(trackerid: Map.get(res, :trackerid, ""))
    |> Ecto.Changeset.change(last_announce: DateTime.utc_now() |> DateTime.truncate(:second))
    |> Ecto.Changeset.change(next_announce: DateTime.utc_now() |> Timex.shift(seconds: res.interval) |> DateTime.truncate(:second))
    |> Repo.update!()

    changesets = Enum.map(res.peers, fn peer ->
      %{
        torrent_id: torrent.id,
        address: %Postgrex.INET{address: peer.ip},
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)
    |> Enum.filter(fn peer ->
      peer.port > 0
    end)

    Repo.insert_all(Peer, changesets, on_conflict: :nothing)

    {:noreply, {info_hash, timer}}
  end

  def terminate(_reason, {_hash, timer}) do
    if not is_nil(timer) do
      Process.cancel_timer(timer)
    end
    :ok
  end
end
