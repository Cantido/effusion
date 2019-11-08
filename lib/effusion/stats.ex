defmodule Effusion.Stats do
  alias Effusion.BTP.{Metainfo, Pieces}
  @moduledoc """
  Utility functions for calculating statistics on a Download.
  """

  def bytes_completed(download) do
    Pieces.bytes_completed(download.pieces)
  end

  def download_size(download) do
    Metainfo.bytes_count(download.meta)
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
