defmodule Effusion.BTP.PeerSelection do
  def select_peer(peers) do
    if Enum.empty?(peers) do
      nil
    else
      Enum.random(peers)
    end
  end
end
