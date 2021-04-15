defmodule Effusion.DHT.Messages.GetPeers.Response do
  defstruct [
    :sender_id,
    :token,
    :values,
    :nodes
  ]

  def nodes_response(sender_id, nodes) do
    %__MODULE__{
      sender_id: sender_id,
      token: token(),
      nodes: nodes
    }
  end

  def peers_response(sender_id, peers) do
    %__MODULE__{
      sender_id: sender_id,
      token: token(),
      values: peers
    }
  end

  defp token do
    Base.encode64(:crypto.strong_rand_bytes(6))
  end
end
