defmodule Effusion.Torrents do
  use GenServer, restart: :transient
  alias Effusion.Torrent
  alias Effusion.Connections
  require Logger

  def start_child(opts) do
    DynamicSupervisor.start_child(
      Effusion.TorrentSupervisor,
      {__MODULE__, opts}
    )
  end

  def start_link(opts) do
    meta = Keyword.fetch!(opts, :meta)
    GenServer.start_link(__MODULE__, opts, name: via(meta.info_hash))
  end

  def peers(info_hash) do
    GenServer.call(via(info_hash), :get_peers)
  end

  def get_progress(info_hash) do
    GenServer.call(via(info_hash), :get_progress)
  end

  def add_peer(info_hash, address) do
    GenServer.call(via(info_hash), {:add_peer, address})
  end

  def add_data(info_hash, from, index, offset, data) do
    GenServer.call(via(info_hash), {:add_data, from, index, offset, data})
  end

  def get_bitfield(info_hash) do
    GenServer.call(via(info_hash), :get_bitfield)
  end

  def get_meta(info_hash) do
    GenServer.call(via(info_hash), :get_meta)
  end

  def get_block_requests(info_hash, address) do
    GenServer.call(via(info_hash), {:get_block_requests, address})
  end

  def block_requested(info_hash, address, index, offset, size) do
    GenServer.call(via(info_hash), {:block_requested, address, index, offset, size})
  end

  def drop_requests(info_hash, address) do
    GenServer.call(via(info_hash), {:drop_requests, address})
  end

  def peer_has_piece(info_hash, address, piece_index) do
    GenServer.call(via(info_hash), {:peer_has_piece, address, piece_index})
  end

  def piece_verified(info_hash, piece_index) do
    GenServer.call(via(info_hash), {:piece_verified, piece_index})
  end

  def piece_failed_verification(info_hash, piece_index) do
    GenServer.call(via(info_hash), {:piece_failed_verification, piece_index})
  end

  def block_written(info_hash, piece_index, offset) do
    GenServer.call(via(info_hash), {:block_written, piece_index, offset})
  end

  def stop(info_hash) do
    GenServer.call(via(info_hash), :stop)
  end

  defp via(info_hash) do
    {:via, Registry, {TorrentRegistry, info_hash}}
  end

  def init(opts) do
    meta = Keyword.fetch!(opts, :meta)
    {:ok, %Torrent{meta: meta}, {:continue, :announce}}
  end

  def handle_continue(:announce, torrent) do
    request =
      %Effusion.Tracker.Request{
        url: torrent.meta.announce,
        ip: Application.fetch_env!(:effusion, :host),
        port: Application.fetch_env!(:effusion, :port),
        peer_id: Application.fetch_env!(:effusion, :peer_id),
        info_hash: torrent.meta.info_hash,
        uploaded: torrent.bytes_uploaded,
        downloaded: torrent.bytes_downloaded,
        left: Torrent.bytes_left(torrent),
        event: "started",
        numwant: Application.fetch_env!(:effusion, :max_peers)
      }

    Honeydew.async({:announce, [request]}, :tracker)

    {:noreply, torrent}
  end

  def handle_call(:get_progress, _from, torrent) do
    {:reply, Torrent.progress(torrent), torrent}
  end

  def handle_call(:get_meta, _from, torrent) do
    {:reply, torrent.meta, torrent}
  end

  def handle_call(:get_bitfield, _from, torrent) do
    {:reply, Torrent.get_bitfield(torrent), torrent}
  end

  def handle_call({:add_data, from, index, offset, data}, _from, torrent) do
    torrent =
      if Torrent.block_written?(torrent, index, offset) do
        torrent
      else
        Honeydew.async({:write_block, [data, index, offset, torrent.meta]}, :file)

        Torrent.requests_for_block(torrent, index, offset, byte_size(data))
        |> Enum.reject(&(&1 == from))
        |> Enum.each(fn address ->
          Honeydew.async({:send, [torrent.meta.info_hash, address, {:cancel, index, offset, byte_size(data)}]}, :connection)
        end)

        Torrent.drop_requests(torrent, index, offset, byte_size(data))
      end

    {:reply, :ok, torrent}
  end

  def handle_call({:piece_verified, index}, _from, torrent) do
    Logger.debug("Verified piece #{index}.")

    torrent = Torrent.piece_verified(torrent, index)
    Honeydew.async({:broadcast, [torrent.meta.info_hash, {:have, index}]}, :connection)

    {:reply, :ok, torrent}
  end

  def handle_call({:piece_failed_verification, index}, _from, torrent) do
    torrent = Torrent.piece_failed_verification(torrent, index)

    {:reply, :ok, torrent}
  end

  def handle_call({:block_written, index, offset}, _from, torrent) do
    torrent = Torrent.block_written(torrent, index, offset)

    if Torrent.piece_written?(torrent, index) do
      Task.Supervisor.start_child(Effusion.TaskSupervisor, fn ->
        piece_size = Effusion.Metadata.piece_size(torrent.meta.info, index)
        read_job = Honeydew.async({:read_block, [index, offset, piece_size, torrent.meta]}, :file, reply: true)
        {:ok, piece_data} = Honeydew.yield(read_job)
        Honeydew.async({:verify_piece, [piece_data, index, torrent.meta]}, :compute)
      end)
    end

    {:reply, :ok, torrent}
  end

  def handle_call(:get_peers, _from, torrent) do
    {:reply, torrent.peers, torrent}
  end

  def handle_call({:add_peer, address}, _from, torrent) do
    {:ok, _pid} = Connections.connect(address, torrent.meta.info_hash)

    {:reply, :ok, Torrent.add_peer(torrent, address)}
  end

  def handle_call({:peer_has_piece, address, piece_index}, _from, torrent) do
    unless Torrent.piece_written?(torrent, piece_index) do
      Honeydew.async({:send, [torrent.meta.info_hash, address, :interested]}, :connection)
    end
    {:reply, :ok, Torrent.peer_has_piece(torrent, address, piece_index)}
  end

  def handle_call({:get_block_requests, address}, _from, torrent) do
    requests = Torrent.block_requests(torrent, address)

    {:reply, requests, torrent}
  end

  def handle_call({:block_requested, address, index, offset, size}, _from, torrent) do
    dl = Torrent.block_requested(torrent, address, index, offset, size)

    {:reply, :ok, dl}
  end

  def handle_call({:drop_requests, address}, _from, torrent) do
    torrent = Torrent.drop_requests(torrent, address)

    {:reply, :ok, torrent}
  end

  def handle_call(:stop, _from, torrent) do
    {:stop, :normal, :ok, torrent}
  end

  def terminate(reason, torrent) do
    Logger.info("Torrent is stopping with reason #{inspect reason}")
    request =
      %Effusion.Tracker.Request{
        url: torrent.meta.announce,
        ip: Application.fetch_env!(:effusion, :host),
        port: Application.fetch_env!(:effusion, :port),
        peer_id: Application.fetch_env!(:effusion, :peer_id),
        info_hash: torrent.meta.info_hash,
        uploaded: torrent.bytes_uploaded,
        downloaded: torrent.bytes_downloaded,
        left: Torrent.bytes_left(torrent),
        event: "stopped",
        numwant: 0
      }

    Honeydew.async({:announce, [request]}, :tracker)
      Connections.disconnect_all(torrent.meta.info_hash)

    :ok
  end
end
