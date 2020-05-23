defmodule Effusion.BTP.VerifierWatchdog do
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Piece
  alias Effusion.BTP.ProtocolHandler
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.Repo
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
      name: {:via, Registry, {VerifierWatchdogRegistry, info_hash}}
    )
  end

  def init(info_hash) do
    Process.flag(:trap_exit, true)
    Logger.debug("VerifierWatchdog started")
    Process.send_after(self(), :watch, @watch_interval_ms)
    {:ok, info_hash}
  end

  def handle_info(:watch, info_hash) do
    Logger.debug("VerifierWatchdog checking for completed pieces")
    verified = Pieces.verified(info_hash)

    verified
    |> Enum.map(fn piece ->
      ConnectionRegistry.btp_broadcast(info_hash, {:have, piece.index})
      Repo.get(Piece, piece.id)
      |> Ecto.Changeset.change(announced: true)
      |> Repo.update!()
    end)

    verified
    |> Enum.each(fn p ->
      Effusion.IO.Server.write_piece(info_hash, p)
      Pieces.mark_piece_written(info_hash, p.index)
    end)

    Logger.debug("VerifierWatchdog announced & wrote #{Enum.count(verified)} pieces")

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
