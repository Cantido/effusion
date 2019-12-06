defmodule Effusion.BTP.DownloadSpeedWatcher do
  alias Effusion.Application.DownloadSpeedWatcherSupervisor
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.ProtocolHandler
  alias Effusion.BTP.PeerSelection
  alias Effusion.Statistics.PeerDownloadAverage
  alias Effusion.Statistics.Net, as: NetStats
  require Logger
  use GenServer

  @moduledoc """
  Connects/disconnects peers based on their download speed.
  """

  @watch_interval_ms 5_000
  @max_bits_per_second :infinity
  @peer_min_bits_per_second 1

  def start(info_hash) do
    case DownloadSpeedWatcherSupervisor.start_child([info_hash]) do
      {:ok, _pid} -> {:ok, info_hash}
      err -> err
    end
  end

  def start_link([info_hash]) do
    GenServer.start_link(__MODULE__, info_hash)
  end

  def init(info_hash) do
    timer = Process.send_after(self(), :watch, @watch_interval_ms)
    {:ok, {info_hash, timer}}
  end

  def handle_info(:watch, {info_hash, timer}) do
    Logger.debug("Download speed watcher checking for slow peers")
    if not is_nil(timer) do
      Process.cancel_timer(timer)
    end
    timer = Process.send_after(self(), :watch, @watch_interval_ms)

    :ok = prune_peers(info_hash)
    :ok = add_peers(info_hash)

    {:noreply, {info_hash, timer}}
  end

  defp prune_peers(info_hash) do
    all_peers = ConnectionRegistry.all_connected(info_hash)

    all_peers
    |> Enum.map(fn peer_id ->
      dl_speed = PeerDownloadAverage.peer_20sec_download_avg(peer_id)

      if dl_speed < @peer_min_bits_per_second do
        Logger.debug("Peer #{peer_id} has a speed of #{dl_speed} which is lower than the threshold of #{@peer_min_bits_per_second}, so we are disconnecting that peer.")
        ProtocolHandler.disconnect(info_hash, peer_id, :slow)
      end
    end)

    :ok
  end

  defp add_peers(info_hash) do
    PeerSelection.select_lowest_failcount(info_hash, 1)
    |> Enum.map(fn peer ->
      address = {peer.address.address, peer.port}
      ProtocolHandler.connect(address, info_hash, peer.peer_id)
    end)
    :ok
  end
end