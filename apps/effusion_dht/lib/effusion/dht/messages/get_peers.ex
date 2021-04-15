defmodule Effusion.DHT.Messages.GetPeers do
  @doc """
  Generates a token for use in `get_peers` responses.
  """
  def token do
    Base.encode64(:crypto.strong_rand_bytes(6))
  end
end
