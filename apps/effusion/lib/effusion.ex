defmodule Effusion do
  import Effusion.Hash, only: [is_hash: 1]

  @moduledoc """
  A BitTorrent library.
  """

  # credo:disable-for-next-line
  @typep hash :: <<_::20, _::_*8>>
  @type info_hash :: hash()
  @type peer_id :: hash()

  @doc """
  Start asynchronously downloading the torrent described by `meta`,
  and write it to a file in the `destfile` directory.

  This function returns immediately.
  """
  def start_download(meta) when is_map(meta) do
    info_hash = meta.info_hash
    block_size = Application.fetch_env!(:effusion, :block_size)
    max_requests_per_peer = Application.fetch_env!(:effusion, :max_requests_per_peer)
    Effusion.Downloads.add(meta)
    Effusion.Downloads.start(
      info_hash,
      block_size,
      max_requests_per_peer,
      Application.fetch_env!(:effusion, :max_half_open_connections),
      Application.fetch_env!(:effusion, :max_peers)
    )
  end

  def stop_download(info_hash) when is_hash(info_hash) do
    Effusion.Downloads.stop(info_hash)
  end
end