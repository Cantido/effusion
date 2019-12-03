defmodule Effusion.THP.Announcer do
  use GenServer

  alias Effusion.Application.AnnouncerSupervisor
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerSelection
  alias Effusion.PWP.TCP.OutgoingHandler
  alias Effusion.BTP.Torrent
  alias Effusion.Repo
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query

  @thp_client Application.get_env(:effusion, :thp_client)

  def start(info_hash) do
    case AnnouncerSupervisor.start_child([info_hash]) do
      {:ok, _pid} -> {:ok, info_hash}
      err -> err
    end
  end

  def start_link([info_hash]) do
    GenServer.start_link(
      __MODULE__,
      info_hash,
      name: {:via, Registry, {AnnouncerRegistry, info_hash}}
    )
  end

  def stop(info_hash) do
    GenServer.call({:via, Registry, {AnnouncerRegistry, info_hash}}, :stop)
  end

  def init(info_hash) do
    {:ok, {info_hash, nil}}
  end

  def announce(info_hash, event) when is_hash(info_hash) do
    GenServer.cast({:via, Registry, {AnnouncerRegistry, info_hash}}, {:announce, event})
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_info(:interval_expired, state) do
    handle_cast({:announce, :interval}, state)
  end

  def handle_cast({:announce, event}, {info_hash, timer}) do
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

    torrent = Repo.one!(from torrent in Torrent,
              where: torrent.info_hash == ^info_hash)
    torrent
    |> Ecto.Changeset.change(trackerid: Map.get(res, :trackerid, ""))
    |> Ecto.Changeset.change(last_announce: Timex.now() |> DateTime.truncate(:second))
    |> Ecto.Changeset.change(next_announce: Timex.now() |> Timex.shift(seconds: res.interval) |> DateTime.truncate(:second))
    |> Repo.update!()

    changesets = Enum.map(res.peers, fn peer ->
      %{
        torrent_id: torrent.id,
        address: %Postgrex.INET{address: peer.ip},
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)

    Repo.insert_all(Peer, changesets, on_conflict: :nothing)

    max_peers = Application.get_env(:effusion, :max_peers)
    eligible_peers = PeerSelection.select_lowest_failcount(info_hash, max_peers)

    Enum.each(eligible_peers, fn p ->
      address = {p.address.address, p.port}
      OutgoingHandler.connect({address, info_hash, peer_id, p.peer_id})
    end)
    {:noreply, {info_hash, timer}}
  end

  def terminate(_reason, {_hash, timer}) do
    if not is_nil(timer) do
      Process.cancel_timer(timer)
    end
    :ok
  end
end
