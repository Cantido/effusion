defmodule Effusion.THP.HTTP do
  @behaviour Effusion.THP

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
    ExBencode.decode(http_res.body)
  end
end
