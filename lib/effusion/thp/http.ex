defmodule Effusion.THP.HTTP do
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.THP.Decode
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  require Logger
  use HTTPoison.Base
  @behaviour Effusion.THP
  @moduledoc """
  An HTTP implementation of the Tracker HTTP Protocol.
  """

  @allowed_opts [
    :compact,
    :event,
    :ip,
    :key,
    :no_peer_id,
    :numwant,
    :trackerid
  ]

  @body_names %{
    "interval" => :interval,
    "peers" => :peers,
    "tracker id" => :trackerid
  }


  def announce(
        tracker_url,
        peer_host,
        peer_port,
        peer_id,
        info_hash,
        uploaded,
        downloaded,
        left,
        opts \\ []
      )
      when is_hash(info_hash) and
             is_peer_id(peer_id) and
             is_integer(peer_port) and
             peer_port in 1..65_535 and
             uploaded >= 0 and
             downloaded >= 0 and
             left >= 0 do
    sanitized_opts = Keyword.take(opts, @allowed_opts) |> Map.new()

    tracker_request = %{
        info_hash: info_hash,
        peer_id: peer_id,
        port: peer_port,
        uploaded: uploaded,
        downloaded: downloaded,
        left: left,
        ip: to_string(:inet.ntoa(peer_host))
      }
      |> Map.merge(sanitized_opts)

    Logger.debug("Making announce to #{tracker_url}")

    query = URI.encode_query(tracker_request)
    case get(tracker_url <> "?" <> query) do
      {:ok, resp} ->
        Logger.debug("Announce to #{tracker_url} successful.")
        {:ok, resp.body}
      {:error, reason} ->
        Logger.error("Announce to #{tracker_url} failed: #{inspect reason}")
        {:error, reason}
    end
  end

  def process_request_url(url) do
    byte_size(url) |> NetStats.add_sent_bytes()
    byte_size(url) |> NetStats.add_sent_tracker_bytes()
    url
  end

  @doc """
  Parses the response body from trackers.

  ## Example

      iex> Effusion.THP.HTTP.process_response_body("d8:intervali1800ee")
      %{interval: 1800, peers: []}
  """
  def process_response_body(body) do
    case ExBencode.decode(body) do
      {:ok, bterm} ->
          bterm
          |> Effusion.Map.rename_keys(@body_names)
          |> Map.update(:peers, [], &Decode.decode_peers/1)
      _err ->
        raise "tracker response body had bad bencoding"
    end
  end

  def process_response_headers(headers) do
    {length, ""} = headers["content-length"] |> Integer.parse()
    length |> NetStats.add_recv_bytes()
    length |> NetStats.add_recv_tracker_bytes()
    headers
  end
end
