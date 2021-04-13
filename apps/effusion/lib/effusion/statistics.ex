defmodule Effusion.Statistics do
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats

  def init do
    NetStats.init()
    PeerStats.init()
    SessionStats.init()
  end
end
