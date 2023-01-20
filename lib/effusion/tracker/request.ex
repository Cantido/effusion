defmodule Effusion.Tracker.Request do

  @enforce_keys [
    :info_hash,
    :peer_id,
    :port,
    :uploaded,
    :downloaded,
    :left,
    :ip
  ]
  defstruct [
    :url,
    :ip,
    :port,
    :peer_id,
    :info_hash,
    :uploaded,
    :downloaded,
    :left,
    :compact,
    :event,
    :key,
    :no_peer_id,
    :numwant,
    :trackerid
  ]
end
