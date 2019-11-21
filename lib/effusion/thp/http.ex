defmodule Effusion.THP.HTTP do
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.THP.Decode
  import Effusion.BTP.Peer
  import Effusion.Hash
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
    http_client = Keyword.get(opts, :http_client, HTTPotion)
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
    http_res = http_client.get(tracker_url <> "?" <> query)
    record_request_stats(query, http_res)
    Logger.debug("Announce to #{tracker_url} successful.")

    if http_res.status_code == 200 do
      case ExBencode.decode(http_res.body) do
        {:ok, bterm} ->
          tokenized =
            bterm
            |> Effusion.Map.rename_keys(@body_names)
            |> Map.update(:peers, [], &Decode.decode_peers/1)

          {:ok, tokenized}

        err ->
          err
      end
    else
      {:error, {:http_request_failed, http_res.status_code, http_res}}
    end
  end

  def record_request_stats(query, response) do
    byte_size(query) |> NetStats.add_sent_bytes()
    byte_size(query) |> NetStats.add_sent_tracker_bytes()
    {length, ""} = response.headers["content-length"] |> Integer.parse()
    length |> NetStats.add_recv_bytes()
    length |> NetStats.add_recv_tracker_bytes()
  end
end
