defmodule Effusion.DownloadManager do
  use GenServer
  alias Effusion.Download
  alias Effusion.Piece
  alias Effusion.Peer
  alias Effusion.PeerManager
  alias Effusion.TCPWorker
  require Logger

  def start_link(opts) do
    meta = Keyword.fetch!(opts, :meta)
    GenServer.start_link(__MODULE__, opts, name: via(meta.info_hash))
  end

  def peers(info_hash) do
    GenServer.call(via(info_hash), :get_peers)
  end

  def add_peer(info_hash, address) do
    GenServer.call(via(info_hash), {:add_peer, address})
  end

  def get_meta(info_hash) do
    GenServer.call(via(info_hash), :get_meta)
  end

  def peer_has_piece(info_hash, address, piece_index) do
    GenServer.call(via(info_hash), {:peer_has_piece, address, piece_index})
  end

  defp via(info_hash) do
    {:via, Registry, {DownloadRegistry, info_hash}}
  end

  def init(opts) do
    meta = Keyword.fetch!(opts, :meta)
    {:ok, %Download{meta: meta}, {:continue, :announce}}
  end

  def handle_continue(:announce, download) do
    # TODO: Make this a proper worker process, probably with Honeydew
    Task.async(fn ->
      worker = Application.fetch_env!(:effusion, :tracker_worker)

      worker.announce(
        %Effusion.Tracker.Request{
          url: download.meta.announce,
          ip: Application.fetch_env!(:effusion, :host),
          port: Application.fetch_env!(:effusion, :port),
          peer_id: Application.fetch_env!(:effusion, :peer_id),
          info_hash: download.meta.info_hash,
          uploaded: download.bytes_uploaded,
          downloaded: download.bytes_downloaded,
          left: Download.bytes_left(download),
          event: "started",
          numwant: Application.fetch_env!(:effusion, :max_peers)
        }
      )
      |> case do
        {:ok, response} ->
          Enum.each(response.peers, fn response_peer ->
            address = {response_peer.ip, response_peer.port}

            peer = %Peer{
              host: response_peer.ip,
              port: response_peer.port
            }
            PeerManager.add_peer(peer)
            if response_peer[:peer_id] do
              PeerManager.set_peer_id(address, response_peer[:peer_id])
            end

            add_peer(download.meta.info_hash, address)
          end)
        err -> Logger.error("tracker returned error: #{inspect err}")
      end
    end)
    {:noreply, download}
  end

  def handle_call(:get_meta, _from, download) do
    {:reply, download.meta, download}
  end

  def handle_call({:add_data, index, offset, data}, _from, download) do
    if Download.piece_written?(download, index) do
      download
    else
      download = Download.add_data(download, index, offset, data)
      piece = Download.get_piece(download, index)
      pid = self()
      if Piece.finished?(piece) do
        # TODO: replace this with a proper worker
        Task.async(fn ->
          data = Piece.data(piece)
          with :ok <- Effusion.IO.write_piece(data, index, download.meta.info) do
            GenServer.call(pid, {:piece_written, index})
          end
        end)
      end

      download
    end

    {:reply, :ok, download}
  end

  def handle_call({:piece_written, index}, _from, download) do
    download = Download.piece_written(download, index)
    {:reply, :ok, download}
  end

  def handle_call(:get_peers, _from, download) do
    {:reply, download.peers, download}
  end

  def handle_call({:add_peer, address}, _from, download) do
    {:ok, _pid} = TCPWorker.connect(address, download.meta.info_hash)

    {:reply, :ok, Download.add_peer(download, address)}
  end

  def handle_call({:peer_has_piece, address, piece_index}, _from, download) do
    unless Download.piece_written?(download, piece_index) do
      Task.async(fn ->
        TCPWorker.send(download.meta.info_hash, address, :interested)
      end)
    end
    {:reply, :ok, Download.peer_has_piece(download, address, download.meta.info_hash)}
  end
end
