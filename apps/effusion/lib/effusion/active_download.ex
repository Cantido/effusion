defmodule Effusion.ActiveDownload do
  use GenServer
  alias Effusion.Download
  alias Effusion.Piece
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

  def add_data(info_hash, index, offset, data) do
    GenServer.call(via(info_hash), {:add_data, index, offset, data})
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

  def peer_has_piece(info_hash, address, piece_index) do
    GenServer.call(via(info_hash), {:peer_has_piece, address, piece_index})
  end

  def piece_written(info_hash, piece_index) do
    GenServer.call(via(info_hash), {:piece_written, piece_index})
  end

  def stop(info_hash) do
    GenServer.call(via(info_hash), :stop)
  end

  defp via(info_hash) do
    {:via, Registry, {DownloadRegistry, info_hash}}
  end

  def init(opts) do
    meta = Keyword.fetch!(opts, :meta)
    {:ok, %Download{meta: meta}, {:continue, :announce}}
  end

  def handle_continue(:announce, download) do
    request =
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

    Honeydew.async({:announce, [request]}, :tracker)

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
      if Piece.finished?(piece) do

        data = Piece.data(piece)
        Honeydew.async({:write_piece, [data, index, download.meta]}, :file)
        Honeydew.async({:broadcast, [download.meta.info_hash, {:have, index}]}, :connection)
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
      Honeydew.async({:send, [download.meta.info_hash, address, :interested]}, :connection)
    end
    {:reply, :ok, Download.peer_has_piece(download, address, piece_index)}
  end

  def handle_call({:get_block_requests, address}, _from, download) do
    requests = Download.block_requests(download, address)

    {:reply, requests, download}
  end

  def handle_call({:block_requested, address, index, offset, size}, _from, download) do
    dl = Download.block_requested(download, address, index, offset, size)

    {:reply, :ok, dl}
  end

  def handle_call(:stop, _from, download) do
    {:stop, :normal, :ok, download}
  end

  def terminate(_, download) do
    request =
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

    Honeydew.async({:announce, [request]}, :tracker)
    TCPWorker.disconnect_all(download.meta.info_hash)

    :ok
  end
end
