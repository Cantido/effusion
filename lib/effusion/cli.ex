defmodule Effusion.CLI do
  alias Effusion.BTP.Metainfo
  alias Effusion.Format
  alias Effusion.Stats
  use Timex

  @moduledoc """
  A command-line interface to Effusion.
  """

  @aliases [o: :output]
  @strict [output: :string]

  @doc """
  Download a file.

  Usage: `effusion <name> -o <destination>`
  """
  def main(argv \\ []) do
    {opts, [file], invalid} =
      OptionParser.parse(argv, strict: @strict, aliases: @aliases)

    Enum.each(invalid, fn i ->
      IO.warn("Invalid option #{i}")
    end)

    {:ok, metabin} = file |> Path.expand() |> File.read()
    {:ok, meta} = Metainfo.decode(metabin)
    dest = Keyword.get(opts, :output, meta.info.name) |> Path.expand

    {:ok, info_hash} = Effusion.start_download(meta, dest)


    output_loop(info_hash)
  end

  @name_width 40
  @progress_width 20
  @percent_width 10
  @downloaded_width 14
  @duration_width 20

  defp output_loop(info_hash) do
    download = Effusion.BTP.DownloadServer.get(info_hash)
    dur = Stats.download_duration(download)

    {downloaded, total_to_download} = Stats.downloaded_ratio(download)
    fraction_downloaded = downloaded / total_to_download

    name_formatted = download.meta.info.name
    percent_downloaded = Float.round(fraction_downloaded * 100, 3)
    progress_bar = Format.progress_bar(percent_downloaded, @progress_width - 2)
    downloaded_str = Format.bytes(downloaded)
    duration_formatted = Timex.format_duration(dur, :humanized)

    IO.write IO.ANSI.clear()
    IO.write IO.ANSI.cursor(0, 0)

    IO.puts row("NAME", "PROGRESS", "PERCENT", "DOWNLOADED", "DURATION")
    IO.puts row(name_formatted, progress_bar, percent_downloaded, downloaded_str, duration_formatted)

    Process.sleep(100)
    output_loop(info_hash)
  end


  defp row(name, progress, percent, downloaded, duration) do
    String.pad_trailing(name, @name_width - 1) <> "|" <>
      String.pad_trailing(" #{progress}", @progress_width - 1) <> "|" <>
      String.pad_trailing(" #{percent}", @percent_width - 1) <> "|" <>
      String.pad_trailing(" #{downloaded}", @downloaded_width - 1) <> "|" <>
      String.pad_trailing(" #{duration}", @duration_width - 1)
  end
end
