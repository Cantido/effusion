defmodule Effusion.BTP.PeerSelection do
  def select_peer(peers) do
    if Enum.empty?(peers) do
      nil
    else
      Enum.random(select_lowest_failcount_category(peers))
    end
  end

  defp select_lowest_failcount_category(peers) do
    peers
    |> Enum.chunk_by(&(&1.failcount))
    |> Enum.sort_by(&List.first(&1).failcount)
    |> List.first()
  end
end
