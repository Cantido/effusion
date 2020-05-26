defmodule Effusion.BTP.ProtocolHandler do
  use GenServer
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.ProtocolHandler
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.THP.Announcer
  alias Effusion.Repo
  require Logger

  @moduledoc """
  Responds to BitTorrent Protocol events.
  """

  ## API

  @doc """
  Start the session server and link it to the current process.
  """
  def start_link(info_hash) do
    GenServer.start_link(
      __MODULE__,
      info_hash,
      name: {:via, Registry, {BTPHandlerRegistry, info_hash}}
    )
  end

  def get(info_hash) do
    GenServer.call({:via, Registry, {BTPHandlerRegistry, info_hash}}, :get, 10_000)
  end

  def have_piece(info_hash, piece) do
    GenServer.call({:via, Registry, {BTPHandlerRegistry, info_hash}}, {:have_piece, piece})
  end

  def notify_all_pieces_written(info_hash) do
    GenServer.call({:via, Registry, {BTPHandlerRegistry, info_hash}}, :all_pieces_written)
  end

  @doc """
  Wait on a download managed by a session server to complete.
  """
  @spec await(binary()) :: :ok | {:error, term()}
  def await(info_hash) do
    GenServer.call({:via, Registry, {BTPHandlerRegistry, info_hash}}, :await, :infinity)
  end

  @doc """
  Start or restart a download
  """
  def start(info_hash) do
    GenServer.call({:via, Registry, {BTPHandlerRegistry, info_hash}}, :start)
  end

  ## Callbacks

  def init(info_hash) do
    Process.flag(:trap_exit, true)
    state = %{
      info_hash: info_hash,
      listeners: MapSet.new()
    }

    {:ok, state}
  end

  def handle_call({:have_piece, piece}, _from, d) do
    Logger.debug("Telling peers that we have piece #{piece.index} of #{Effusion.Hash.encode d.info_hash}")
    ConnectionRegistry.btp_broadcast(d.info_hash, {:have, piece.index})
    Repo.get(Piece, piece.id)
    |> Ecto.Changeset.change(announced: true)
    |> Repo.update!()

    {:reply, :ok, d}
  end

  def handle_call(:all_pieces_written, _from, d) do
    :telemetry.execute([:btp, :completed], %{}, %{info_hash: d.info_hash})
    :ok = Announcer.announce(d.info_hash, :completed)

    Torrent.by_info_hash!(d.info_hash)
    |> Torrent.finish()
    |> Repo.update()

    reply_to_listeners(d, :ok)

    {:reply, :ok, d}
  end

  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  def handle_call(:await, from, state) do
    state = Map.update(state, :listeners, MapSet.new(), &MapSet.put(&1, from))
    {:noreply, state}
  end

  def handle_call(:start, _from, state) do
    :telemetry.execute([:btp, :started], %{}, %{info_hash: state.info_hash})

    Torrent.by_info_hash!(state.info_hash)
    |> Torrent.start(DateTime.utc_now())
    |> Repo.update()

    :ok = Announcer.announce(state.info_hash, :started)
    {:reply, :ok, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(reason, state) do
    Logger.debug("download server terminating with reason: #{inspect(reason)}")
    ProtocolHandler.disconnect_all(state.info_hash)

    :ok = Announcer.announce(state.info_hash, :stopped)

    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state, msg) do
    Enum.each(state.listeners, fn l -> GenServer.reply(l, msg) end)
  end
end
