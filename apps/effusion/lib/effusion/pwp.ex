defmodule Effusion.PWP do
  defguard is_peer_id(term) when not is_nil(term) and is_binary(term) and byte_size(term) == 20
end
