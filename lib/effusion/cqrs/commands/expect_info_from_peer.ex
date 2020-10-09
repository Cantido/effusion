defmodule Effusion.CQRS.Commands.ExpectInfoFromPeer do
  defstruct [
    :peer_uuid,
    :info_hash,
    :peer_id
  ]
end
