defmodule Effusion.DownloadManager do
  use GenServer
  alias Effusion.Download
  alias Effusion.Piece

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    meta = Keyword.fetch!(opts, :meta)
    {:ok, %Download{meta: meta}}
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
end
