defmodule Effusion.THP.HTTP do
  alias Effusion.Statistics.Net, as: NetStats
  import Effusion.BTP.Peer
  import Effusion.Hash
  require Logger
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
    {:ok, http_res} = http_client.get(tracker_url <> "?" <> query)
    record_request_stats(query, http_res)
    Logger.debug("Announce to #{tracker_url} successful.")
    decode_response(http_res)
  end

  def record_request_stats(query, response) do
    byte_size(query) |> NetStats.add_sent_bytes()
    byte_size(query) |> NetStats.add_sent_tracker_bytes()
    {length, ""} = response.headers["content-length"] |> Integer.parse()
    length |> NetStats.add_recv_bytes()
    length |> NetStats.add_recv_tracker_bytes()
  end

  defp decode_response(http_res) do
    if http_res.status_code == 200 do
      decode(http_res.body)
    else
      {:error, {:http_request_failed, http_res.status_code, http_res}}
    end
  end

  @body_names %{
    "interval" => :interval,
    "peers" => :peers,
    "tracker id" => :trackerid
  }

  @peer_names %{
    "ip" => :ip,
    "port" => :port,
    "peer id" => :peer_id
  }

  @doc """
  Decode a tracker response.
  """
  def decode(body) do
    case ExBencode.decode(body) do
      {:ok, bterm} ->
        tokenized =
          bterm
          |> Effusion.Map.rename_keys(@body_names)
          |> Map.update(:peers, [], &update_peers/1)

        {:ok, tokenized}

      err ->
        err
    end
  end

  defp update_peers(""), do: []
  defp update_peers([]), do: []

  defp update_peers(peers) when is_binary(peers) do
    peers
    |> :binary.bin_to_list()
    |> Enum.chunk_every(6)
    |> Enum.map(fn [ip0, ip1, ip2, ip3, p0, p1] ->
      <<port::16>> = <<p0, p1>>

      %{
        ip: {ip0, ip1, ip2, ip3},
        port: port
      }
    end)
  end

  defp update_peers(peers) when is_list(peers) do
    Enum.map(peers, &update_peer/1)
  end

  defp update_peer(peer) do
    peer
    |> Effusion.Map.rename_keys(@peer_names)
    |> Map.update!(:ip, &parse_address/1)
  end

  defp parse_address(addr) do
    with addr_charlist <- to_charlist(addr),
         {:ok, ip} <- :inet.parse_address(addr_charlist) do
      ip
    end
  end
end
