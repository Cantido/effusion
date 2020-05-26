defmodule Effusion.BTP.FinishedTorrentWatchdog do
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.ProtocolHandler
  require Logger
  use GenServer
  @moduledoc """
  Watches for completed pieces.

  Any pieces that get verified will get announced and written.
  """

  @watch_interval_ms 250

  def start_link(info_hash) do
    GenServer.start_link(
      __MODULE__,
      info_hash,
      name: {:via, Registry, {FinishedTorrentWatchdogRegistry, info_hash}}
    )
  end

  def init(info_hash) do
    Process.flag(:trap_exit, true)
    Logger.debug("FinishedTorrentWatchdog started")
    Process.send_after(self(), :watch, @watch_interval_ms)
    {:ok, info_hash}
  end

  def handle_info(:watch, info_hash) do
    if Pieces.all_written?(info_hash) && !Torrent.finished?(info_hash) do
      Logger.debug("All pieces are written, notifying BTP handler")
      ProtocolHandler.notify_all_pieces_written(info_hash)
    else
      Process.send_after(self(), :watch, @watch_interval_ms)
    end

    {:noreply, info_hash}
  end

  def terminate(reason, info_hash) do
    Logger.debug("Watchdog for #{info_hash |> Effusion.Hash.encode} terminating for reason: #{inspect reason}")
    :ok
  end
end