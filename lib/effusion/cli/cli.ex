defmodule Effusion.CLI do
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Torrent
  alias Effusion.Format
  alias Effusion.Repo
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats
  alias Effusion.Statistics.SessionDownloadAverage
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

    files
    |> Enum.map(fn file ->
      contents = File.read!(file)
      {:ok, meta} = Metatorrent.decode(contents)
      Effusion.CQRS.Contexts.Downloads.add(meta)
      Effusion.CQRS.Contexts.Downloads.start(
        meta.info_hash,
        Application.fetch_env!(:effusion, :block_size),
        Application.fetch_env!(:effusion, :max_requests_per_peer),
        Application.fetch_env!(:effusion, :max_half_open_connections),
        Application.fetch_env!(:effusion, :max_peers)
      )
    end)

    Process.sleep(1000)

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

    info_hashes = Repo.all(
      from torrent in Torrent,
      where: torrent.state == "downloading",
      select: torrent.info_hash
    )

    torrent_rows = Enum.map(info_hashes, fn info_hash ->
      torrent_row(info_hash)
    end)

    this_loop_time = System.monotonic_time(:millisecond)
    seconds_since_last_loop = max(this_loop_time - last_timestamp, 1) / 1_000

    downloaded_payload_bytes = NetStats.recv_payload_bytes()
    uploaded_bytes = NetStats.sent_bytes()
    dl_speed = SessionDownloadAverage.session_01sec_download_avg()
    ul_speed = (uploaded_bytes - last_uploaded_bytes) / seconds_since_last_loop

    dl_speed_formatted = dl_speed |> trunc() |> Format.bytes()
    ul_speed_formatted = ul_speed |> trunc() |> Format.bytes()

    dl_bytes_formatted = downloaded_payload_bytes |> Format.bytes()
    ul_bytes_formatted = uploaded_bytes |> Format.bytes()

    IO.write(IO.ANSI.clear())
    IO.write(IO.ANSI.cursor(0, 0))

    IO.puts(row("NAME", "PROGRESS", "PERCENT", "DOWNLOADED", "DURATION"))

    Enum.each(torrent_rows, fn tr ->
      IO.puts(tr)
    end)

    IO.puts("")

    IO.puts(
      "Down: #{dl_speed_formatted}/s (#{dl_bytes_formatted}); " <>
      "Up: #{ul_speed_formatted}/s (#{ul_bytes_formatted})"
    )

    if NetStats.has_incoming_connections?() do
      IO.puts("Receiving connections")
    else
      IO.puts("No incoming connections!")
    end

    IO.puts("Total TCP connections: #{PeerStats.num_tcp_peers()}")
    IO.puts("Total half-open connections: #{PeerStats.num_peers_half_open()}")

    IO.puts("---INCOMING---")
    IO.puts("choke:          #{SessionStats.num_incoming_choke()}")
    IO.puts("unchoke:        #{SessionStats.num_incoming_unchoke()}")
    IO.puts("interested:     #{SessionStats.num_incoming_interested()}")
    IO.puts("not interested: #{SessionStats.num_incoming_not_interested()}")
    IO.puts("have:           #{SessionStats.num_incoming_have()}")
    IO.puts("bitfield:       #{SessionStats.num_incoming_bitfield()}")
    IO.puts("request:        #{SessionStats.num_incoming_request()}")
    IO.puts("piece:          #{SessionStats.num_incoming_piece()}")
    IO.puts("cancel:         #{SessionStats.num_incoming_cancel()}")

    IO.puts("---OUTGOING---")
    IO.puts("choke:          #{SessionStats.num_outgoing_choke()}")
    IO.puts("unchoke:        #{SessionStats.num_outgoing_unchoke()}")
    IO.puts("interested:     #{SessionStats.num_outgoing_interested()}")
    IO.puts("not interested: #{SessionStats.num_outgoing_not_interested()}")
    IO.puts("have:           #{SessionStats.num_outgoing_have()}")
    IO.puts("bitfield:       #{SessionStats.num_outgoing_bitfield()}")
    IO.puts("request:        #{SessionStats.num_outgoing_request()}")
    IO.puts("piece:          #{SessionStats.num_outgoing_piece()}")
    IO.puts("cancel:         #{SessionStats.num_outgoing_cancel()}")


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
