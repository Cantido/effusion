defmodule Effusion.Statistics.Peer do
  def init do
    :ets.new(PeerStatsTable, [:set, :public, :named_table, read_concurrency: false, write_concurrency: true])
  end

  def inc_num_tcp_peers do
    :ets.update_counter(PeerStatsTable, :num_tcp_peers, 1, {:k, 0})
  end

  def dec_num_tcp_peers do
    :ets.update_counter(PeerStatsTable, :num_tcp_peers, {2, -1, 0, 0})
  end

  def num_tcp_peers do
    :ets.lookup_element(PeerStatsTable, :num_tcp_peers, 2)
  end
end
