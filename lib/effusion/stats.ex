defmodule Effusion.Stats do
  def bytes_completed(download) do
    Effusion.BTP.Pieces.bytes_completed(download.pieces)
  end

  def download_size(download) do
    Effusion.BTP.Metainfo.bytes_count(download.meta)
  end

  def downloaded_ratio(download) do
    {
      bytes_completed(download),
      download_size(download)
    }
  end

  def download_duration(download) do
    Timex.Interval.new(from: download.started_at, until: Timex.now()) |> Timex.Interval.duration(:duration)
  end
end
