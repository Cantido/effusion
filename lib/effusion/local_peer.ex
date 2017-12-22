alias Effusion.InfoHash

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
  @spec peer_id() :: InfoHash.t
  def peer_id do
    <<0 :: integer-size(160)>>
  end
end
