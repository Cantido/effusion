defmodule Effusion.THP.HTTP do
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
    event \\ :interval
  ) do
    tracker_request = %{
      info_hash: info_hash,
      peer_id: peer_id,
      port: peer_port,
      uploaded: uploaded,
      downloaded: downloaded,
      left: left,
      ip: to_string(:inet.ntoa(peer_host))
    }

    with {:ok, event} <- build_event_param(event),
         tracker_request = Map.put(tracker_request, :event, event),
         http_res = HTTPotion.get(tracker_url <> "?" <> URI.encode_query(tracker_request))
    do
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
    "peers" => :peers
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
        tokenized = bterm
        |> Effusion.Map.rename_keys(@body_names)
        |> Map.update(:peers, [], &update_peers/1)
        {:ok, tokenized}
      err -> err
    end
  end

  defp update_peers(peers) do
    Enum.map(peers, &update_peer/1)
  end

  defp update_peer(peer) do
    peer
      |> Effusion.Map.rename_keys(@peer_names)
      |> Map.update!(:ip, &parse_address/1)
  end

  defp parse_address(addr) do
    with addr_charlist <- to_charlist(addr),
         {:ok, ip} <- :inet.parse_address(addr_charlist)
    do
      ip
    end
  end
end
