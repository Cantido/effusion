defmodule Effusion.CLI do
  alias Effusion.BTP.DownloadServer
  alias Effusion.BTP.Download
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Swarm
  alias Effusion.Format
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.PeerDownloadAverage
  alias Effusion.Statistics.SessionDownloadAverage
  alias Effusion.Repo
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
    dest = Keyword.get(opts, :output, meta.info.name) |> Path.expand()

    {:ok, info_hash} = Effusion.start_download(meta, dest)

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
    download = DownloadServer.get(info_hash)
    dur = Download.download_duration(download)

    {downloaded, total_to_download} = Download.downloaded_ratio(download)
    fraction_downloaded = downloaded / total_to_download

    name_formatted = download.meta.info.name
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
    Enum.each(Swarm.peers(), fn peer ->
      if Peer.connected?(peer, info_hash) do
        peer = peer |> Repo.preload(:blocks_we_requested)
        IO.puts "#{inspect peer.peer_id} -- #{PeerDownloadAverage.peer_20sec_download_avg(peer.peer_id) |> trunc() |> Format.bytes()}/s --- Requested #{peer.blocks_we_requested |> Enum.count} blocks"
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