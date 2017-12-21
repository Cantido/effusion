defmodule Effusion.LocalPeer do
  @spec peer_id() :: binary()
  def peer_id do
    <<0 :: integer-size(160)>>
  end
end
