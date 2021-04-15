defmodule Effusion.DHT.Messages.AnnouncePeer.Query do
  defstruct [
    :sender_id,
    :info_hash,
    :port,
    :token,
    :implied_port
  ]
end
