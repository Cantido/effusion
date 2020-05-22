defmodule Effusion.CLI do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.Format
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.PeerDownloadAverage
  alias Effusion.Statistics.SessionDownloadAverage
  alias Effusion.Repo
  import Ecto.Query
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
    {_opts, files, invalid} = OptionParser.parse(argv, strict: @strict, aliases: @aliases)

    Enum.each(invalid, fn i ->
      IO.warn("Invalid option #{i}")
    end)

    if(Enum.any?(files)) do
      Enum.each(files, fn file ->
        {:ok, metabin} = file |> Path.expand() |> File.read()
        {:ok, meta} = Metatorrent.decode(metabin)

        Repo.update_all(Effusion.BTP.Torrent, set: [
          state: "paused"
        ])

        Repo.get_by!(Effusion.BTP.Torrent, [info_hash: meta.info_hash])
        |> Ecto.Changeset.change(state: "downloading")
        |> Effusion.Repo.update!()
      end)
    end

    Effusion.BTP.Session.start_link([])

    Process.sleep(100)

    output_loop()
  end

  @name_width 40
  @progress_width 20
  @percent_width 10
  @downloaded_width 14
  @duration_width 20

  defp output_loop(
         last_uploaded_bytes \\ 0,
         last_timestamp \\ System.monotonic_time(:millisecond)
       ) do

    info_hashes = Repo.all(from torrent in Torrent,
                           where: torrent.state == "downloading",
                           select: torrent.info_hash)

    torrent_rows = Enum.map(info_hashes, fn info_hash ->
      torrent_row(info_hash)
    end)

    this_loop_time = System.monotonic_time(:millisecond)
    time_since_last_loop = max(this_loop_time - last_timestamp, 1)

    downloaded_bytes = NetStats.recv_bytes()
    uploaded_bytes = NetStats.sent_bytes()
    dl_speed = SessionDownloadAverage.session_20sec_download_avg()
    ul_speed = (uploaded_bytes - last_uploaded_bytes) / time_since_last_loop * 1_000

    dl_speed_formatted = dl_speed |> trunc() |> Format.bytes()
    ul_speed_formatted = ul_speed |> trunc() |> Format.bytes()

    dl_bytes_formatted = downloaded_bytes |> Format.bytes()
    ul_bytes_formatted = uploaded_bytes |> Format.bytes()

    IO.write(IO.ANSI.clear())
    IO.write(IO.ANSI.cursor(0, 0))

    IO.puts(row("NAME", "PROGRESS", "PERCENT", "DOWNLOADED", "DURATION"))

    Enum.each(torrent_rows, fn tr ->
      IO.puts(tr)
    end)

    IO.puts("")

    IO.puts(
      "Down: #{dl_speed_formatted}/s (#{dl_bytes_formatted}); Up: #{ul_speed_formatted}/s (#{
        ul_bytes_formatted
      })"
    )

    if NetStats.has_incoming_connections?() do
      IO.puts("Receiving connections")
    else
      IO.puts("No incoming connections!")
    end

    IO.puts("Total TCP connections: #{PeerStats.num_tcp_peers()}")
    IO.puts("Total half-open connections: #{PeerStats.num_peers_half_open()}")

    Process.sleep(100)
    output_loop(uploaded_bytes, this_loop_time)
  end

  defp torrent_row(info_hash) do
    torrent = Repo.one!(from torrent in Torrent,
                        where: torrent.info_hash == ^info_hash,
                        select: torrent)

    dur = Timex.Interval.new(from: torrent.started, until: DateTime.utc_now()) |> Timex.Interval.duration(:duration)

    downloaded = Pieces.bytes_completed(info_hash)
    total_to_download = Pieces.torrent_length(info_hash)
    fraction_downloaded = downloaded / total_to_download

    name_formatted = torrent.name
    percent_downloaded = Float.round(fraction_downloaded * 100, 3)
    progress_bar = Format.progress_bar(percent_downloaded, @progress_width - 2)
    downloaded_str = Format.bytes(downloaded)
    duration_formatted = Timex.format_duration(dur)

    row(name_formatted, progress_bar, percent_downloaded, downloaded_str, duration_formatted)
  end

  defp row(name, progress, percent, downloaded, duration) do
    constrain_text(name, @name_width) <>
      "| " <>
      constrain_text(progress, @progress_width) <>
      "| " <>
      constrain_text(percent, @percent_width) <>
      "| " <>
      constrain_text(downloaded, @downloaded_width) <>
      "| " <> constrain_text(duration, @duration_width)
  end

  defp constrain_text(string, length) do
    string
    |> to_string()
    |> String.slice(0, length)
    |> String.pad_trailing(length)
  end
end
