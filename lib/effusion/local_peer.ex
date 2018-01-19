alias Effusion.PeerId

defmodule Effusion.LocalPeer do
  @moduledoc """
  The peer that is acting on behalf of this software's user.
  """

  @doc """
  Returns the local peer's peer ID. Currently stubbed-out to a constant.

  ## Example

      iex> Effusion.LocalPeer.peer_id()
      "Effusion Experiment!"

  """
  @spec peer_id() :: PeerId.t
  def peer_id do
    "Effusion Experiment!"
  end

  def matches_id?(id) do
    peer_id() == id
  end

  def ip_address() do
    {127, 0, 0, 1}
  end
end
