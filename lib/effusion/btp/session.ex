defmodule Effusion.BTP.Session do
  use GenServer
  alias Effusion.BTP.Torrent
  alias Effusion.Repo
  import Ecto.Query
  require Logger

  @moduledoc """
  The global state of the application.

  This module resumes all torrents on application startup.
  """

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    {:ok, [], 0}
  end

  def handle_info(:timeout, []) do
    Repo.delete_all(Effusion.BTP.PeerPiece)
    Repo.delete_all(Effusion.BTP.Request)
    Repo.update_all(Effusion.BTP.Peer, set: [
      failcount: 0,
      connected: false,
      peer_choking: true,
      peer_interested: false,
      am_choking: true,
      am_interested: false
    ])
    :ok = resume_torrents()
    {:noreply, []}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def resume_torrents do
    torrents_to_resume = Repo.all(
      from torrent in Torrent,
      where: torrent.state == "downloading",
      select: torrent.info_hash
    )
    Enum.each(torrents_to_resume, fn info_hash ->
      Logger.info("Resuming torrent #{info_hash |> Effusion.Hash.encode()}")
      Task.async(fn ->
        Effusion.start_download(info_hash)
      end)
    end)
    :ok
  end
end
