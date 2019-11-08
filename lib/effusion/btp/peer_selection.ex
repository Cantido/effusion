defmodule Effusion.BTP.PeerSelection do
  @moduledoc """
  Selects which peers to connect to.
  """

  def select_lowest_failcount(peers, count) do
    peers
    |> Enum.sort_by(&(&1.failcount))
    |> Enum.take(count)
  end
end
