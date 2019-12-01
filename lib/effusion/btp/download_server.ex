defmodule Effusion.BTP.DownloadServer do
  use GenServer, restart: :transient
  alias Effusion.Application.DownloadServerSupervisor
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Metainfo.Directory
  alias Effusion.BTP.VerifierWatchdog
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.THP.Announcer
  alias Effusion.Repo
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
