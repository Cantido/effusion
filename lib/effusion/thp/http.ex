defmodule Effusion.THP.HTTP do
  alias Effusion.Statistics.Net, as: NetStats
  @behaviour Effusion.THP
  @moduledoc """
  An HTTP implementation of the Tracker HTTP Protocol.
  """

  def announce(
        tracker_url,
        peer_host,
        peer_port,
        peer_id,
        info_hash,
        uploaded,
        downloaded,
        left,
        event \\ :interval,
        tracker_id \\ ""
      ) do
    tracker_request = %{
      info_hash: info_hash,
      peer_id: peer_id,
      port: peer_port,
      uploaded: uploaded,
      downloaded: downloaded,
      left: left,
      trackerid: tracker_id,
      ip: to_string(:inet.ntoa(peer_host))
    }

    with {:ok, event} <- build_event_param(event),
         tracker_request = Map.put(tracker_request, :event, event),
         query = URI.encode_query(tracker_request),
         http_res = HTTPotion.get(tracker_url <> "?" <> query) do
           byte_size(query) |> NetStats.add_sent_bytes
           byte_size(query) |> NetStats.add_sent_tracker_bytes
           {length, ""} = http_res.headers["content-length"] |> Integer.parse
           length |> NetStats.add_recv_bytes()
           length |> NetStats.add_recv_tracker_bytes()
      decode_response(http_res)
    else
      err -> err
    end
  end

  defp build_event_param(event) do
    case event do
      :started -> {:ok, "started"}
      :stopped -> {:ok, "stopped"}
      :completed -> {:ok, "completed"}
      :interval -> {:ok, ""}
      _ -> {:error, {:bad_event, event}}
    end
  end

  defp decode_response(http_res) do
    if HTTPotion.Response.success?(http_res) do
      decode(http_res.body)
    else
      {:error, {:http_request_failed, http_res.status_code, http_res}}
    end
  end

  @body_names %{
    "interval" => :interval,
    "peers" => :peers,
    "tracker id" => :tracker_id
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
    |> :binary.bin_to_list
    |> Enum.chunk_every(6)
    |> Enum.map(fn [ip0, ip1, ip2, ip3, p0, p1] ->
      << port::16 >> = << p0, p1>>
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
