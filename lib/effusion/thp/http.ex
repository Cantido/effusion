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
    left
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

    http_res = HTTPotion.get(tracker_url <> "?" <> URI.encode_query(tracker_request))
    decode(http_res.body)
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

  def decode(body) do
    {:ok, bterm} = ExBencode.decode(body)

    tokenized = bterm
      |> Effusion.Map.rename_keys(@body_names)
      |> Map.update(:peers, [], &update_peers/1)

    {:ok, tokenized}
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
