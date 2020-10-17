defmodule Effusion.Statistics.Peer do
  @moduledoc """
  Statistics about peers.

  For example, `num_tcp_peers/0` returns the number of peers we are currently connected to.
  """

  @doc """
  Initialize the peer stats ETS table and values.
  """
  def init do
    :ets.new(PeerStatsTable, [
      :set,
      :public,
      :named_table,
      read_concurrency: false,
      write_concurrency: true
    ])

    :ets.insert(PeerStatsTable, [{:num_tcp_peers, 0}])
    :ets.insert(PeerStatsTable, [{:num_peers_half_open, 0}])
  end

  @doc """
  Add a TCP-connected peer.
  """
  def inc_num_tcp_peers do
    :ets.update_counter(PeerStatsTable, :num_tcp_peers, 1, {:k, 0})
  end

  @doc """
  Remove a TCP-connected peer.
  """
  def dec_num_tcp_peers do
    :ets.update_counter(PeerStatsTable, :num_tcp_peers, {2, -1, 0, 0})
  end

  @doc """
  The number of peers currently connected over TCP.
  """
  def num_tcp_peers do
    :ets.lookup_element(PeerStatsTable, :num_tcp_peers, 2)
  end

  @doc """
  Add a half-open TCP connection (i.e. we've started a socket but the remote peer has not yet opened the connection.)
  """
  def inc_num_peers_half_open do
    :ets.update_counter(PeerStatsTable, :num_peers_half_open, 1, {:k, 0})
  end

  @doc """
  Remove a half-open TCP connection (i.e. we've started a socket but the remote peer has not yet opened the connection.)
  """
  def dec_num_peers_half_open do
    :ets.update_counter(PeerStatsTable, :num_peers_half_open, {2, -1, 0, 0})
  end

  @doc """
  The number of half-open TCP connections (i.e. the number of open sockets that the remote peer hasn't connected to yet..)
  """
  def num_peers_half_open do
    :ets.lookup_element(PeerStatsTable, :num_peers_half_open, 2)
  end
end
