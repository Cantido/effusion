defmodule Effusion.Session do
  alias Effusion.Metainfo
  alias Effusion.LocalPeer

  def start(meta_bin) when is_binary(meta_bin) do
    {:ok, meta} = Metainfo.decode(meta_bin)

    tracker_request = %{
      info_hash: meta.info_hash,
      peer_id: LocalPeer.peer_id(),
      port: 4040,
      uploaded: 0,
      downloaded: 0,
      left: 1899528192,
      ip: to_string(:inet.ntoa(LocalPeer.ip_address()))
    }

    http_res = HTTPotion.get("http://localhost:6969?" <> URI.encode_query(tracker_request))
    {:ok, res} = ExBencode.decode(http_res.body)


  end
end
