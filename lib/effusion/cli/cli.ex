defmodule Effusion.CLI do
  alias Effusion.BTP.DownloadServer
  alias Effusion.BTP.Download
  alias Effusion.BTP.Metainfo
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
    {opts, [file], invalid} = OptionParser.parse(argv, strict: @strict, aliases: @aliases)

    Enum.each(invalid, fn i ->
      IO.warn("Invalid option #{i}")
    end)

    {:ok, metabin} = file |> Path.expand() |> File.read()
    {:ok, meta} = Metainfo.decode(metabin)

    {:ok, info_hash} = Effusion.start_download(meta)

    Process.sleep(100)
    output_loop(info_hash)
  end

  @name_width 40
  @progress_width 20
  @percent_width 10
  @downloaded_width 14
  @duration_width 20

  defp output_loop(
         info_hash,
         last_uploaded_bytes \\ 0,
         last_downloaded_bytes \\ 0,
         last_timestamp \\ System.monotonic_time(:millisecond)
       ) do

    torrent = Repo.one!(from torrent in Torrent,
                        where: torrent.info_hash == ^info_hash,
                        select: torrent)

    dur = Timex.Interval.new(from: torrent.started, until: Timex.now()) |> Timex.Interval.duration(:duration)

    downloaded = Pieces.bytes_completed(info_hash)
    total_to_download = Pieces.torrent_length(info_hash)
    fraction_downloaded = downloaded / total_to_download

    name_formatted = torrent.name
    percent_downloaded = Float.round(fraction_downloaded * 100, 3)
    progress_bar = Format.progress_bar(percent_downloaded, @progress_width - 2)
    downloaded_str = Format.bytes(downloaded)
    duration_formatted = Timex.format_duration(dur)

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

    IO.puts(
      row(name_formatted, progress_bar, percent_downloaded, downloaded_str, duration_formatted)
    )

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

    IO.puts("Peers:")

    peers_query = from peer in Peer,
                  left_join: request in Request,
                  on: peer.id == request.peer_id,
                  group_by: peer.id,
                  select: {peer, count(request)}
    peers = Repo.all(peers_query)

    Enum.each(peers, fn {peer, request_count} ->
      if Peer.connected?(peer, info_hash) do
        IO.puts "#{inspect peer.peer_id} -- #{PeerDownloadAverage.peer_20sec_download_avg(peer.peer_id) |> trunc() |> Format.bytes()}/s --- Requested #{request_count} blocks"
      end
    end)

    Process.sleep(100)
    output_loop(info_hash, uploaded_bytes, downloaded_bytes, this_loop_time)
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
