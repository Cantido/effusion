alias Effusion.PeerId

defmodule Effusion.LocalPeer do
  @moduledoc """
  The peer that is acting on behalf of this software's user.
  """

  @doc """
  Returns the local peer's peer ID. Currently stubbed-out to all zeroes.

  ## Example

      iex> Effusion.LocalPeer.peer_id()
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>

  """
  @spec peer_id() :: PeerId.t
  def peer_id do
    <<0 :: integer-size(160)>>
  end

  def matches_id?(id) do
    peer_id() == id
  end
end
