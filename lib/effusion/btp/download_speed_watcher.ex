defmodule Effusion.BTP.DownloadSpeedWatcher do
  alias Effusion.BTP.PeerSelection
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.ProtocolHandler
  alias Effusion.Statistics.PeerDownloadAverage
  require Logger
  use GenServer

  @moduledoc """
  Connects/disconnects peers based on their download speed.
  """

  @watch_interval_ms 10_000
  @peer_min_bits_per_second 1

  @peers_count_to_add_on_speedup Application.fetch_env!(:effusion, :peers_count_to_add_on_speedup)

  def start_link(info_hash) do
    GenServer.start_link(__MODULE__, info_hash)
  end

  def init(info_hash) do
    Process.flag(:trap_exit, true)
    timer = Process.send_after(self(), :watch, @watch_interval_ms)
    {:ok, {info_hash, timer}}
  end

  def handle_info(:watch, {info_hash, timer}) do
    Logger.debug("Download speed watcher checking for slow peers")
    if not is_nil(timer) do
      Process.cancel_timer(timer)
    end
    timer = Process.send_after(self(), :watch, @watch_interval_ms)

    if Torrent.downloading?(info_hash) do
      :ok = prune_peers(info_hash)
      :ok = add_peers(info_hash)
    end

    {:noreply, {info_hash, timer}}
  end

  defp prune_peers(info_hash) do
    all_peers = ConnectionRegistry.all_connected(info_hash)

    all_peers
    |> Enum.each(fn peer_id ->
      dl_speed = PeerDownloadAverage.peer_20sec_download_avg(peer_id)

      if dl_speed < @peer_min_bits_per_second do
        Logger.debug("Peer #{peer_id} has a speed of #{dl_speed} which is lower than the threshold of #{@peer_min_bits_per_second}, so we are disconnecting that peer.")
        ProtocolHandler.disconnect(info_hash, peer_id, :slow)
      end
    end)

    :ok
  end

  defp add_peers(info_hash) do
    PeerSelection.select_lowest_failcount(info_hash, @peers_count_to_add_on_speedup)
    |> Enum.each(fn peer ->
      address = {peer.address.address, peer.port}
      ProtocolHandler.connect(address, info_hash, peer.peer_id)
    end)
    :ok
  end
end
