defmodule Effusion.CLI do
  alias Effusion.BTP.Metainfo
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

  defp output_loop(info_hash) do
    IO.write IO.ANSI.cursor(0, 0)
    IO.write IO.ANSI.clear()

    download = Effusion.BTP.DownloadServer.get(info_hash)
    dur = Timex.Interval.new(from: download.started_at, until: Timex.now()) |> Timex.Interval.duration(:duration)

    downloaded = Effusion.BTP.Pieces.bytes_completed(download.pieces)
    total_to_download = Effusion.BTP.Metainfo.bytes_count(download.meta)
    fraction_downloaded = downloaded / total_to_download

    downloaded_str = format_bytes(downloaded)

    IO.puts "NAME \t\t\t\t\t| PERCENT \t| DOWNLOADED \t| DURATION"
    IO.write "#{download.meta.info.name} \t| #{Float.round(fraction_downloaded * 100, 3)}% \t| #{downloaded_str} \t| #{Timex.format_duration dur, :humanized} "

    Process.sleep(1000)
    output_loop(info_hash)
  end

  @kibibyte 1024
  @mebibyte 1024 * @kibibyte
  @gibibyte 1024 * @mebibyte

  defp format_bytes(n) do
    cond do
      n < @kibibyte -> "#{n} B"
      n < @mebibyte -> "#{Float.round(n/@kibibyte, 1)} kiB"
      n < @gibibyte -> "#{Float.round(n/@mebibyte, 1)} MiB"
      true -> "#{Float.round(n/@mebibyte, 1)} GiB"
    end
  end
end
