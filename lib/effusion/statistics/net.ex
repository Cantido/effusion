defmodule Effusion.Statistics.Net do
  def add_sent_payload_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :sent_payload_bytes, n, {:k, 0})
  end

  def sent_payload_bytes do
    :ets.lookup_element(NetStatsTable, :sent_payload_bytes, 2)
  end

  def add_sent_bytes(n) when is_integer(n) and n >= 0 do
      :ets.update_counter(NetStatsTable, :sent_bytes, n, {:k, 0})
  end

  def sent_bytes do
    :ets.lookup_element(NetStatsTable, :sent_bytes, 2)
  end

  def add_sent_ip_overhead_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :sent_ip_overhead_bytes, n, {:k, 0})
  end

  def sent_ip_overhead_bytes do
    :ets.lookup_element(NetStatsTable, :sent_ip_overhead_bytes, 2)
  end

  def add_sent_tracker_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :sent_tracker_bytes, n, {:k, 0})
  end

  def sent_tracker_bytes do
    :ets.lookup_element(NetStatsTable, :sent_tracker_bytes, 2)
  end



  def add_recv_payload_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :recv_payload_bytes, n, {:k, 0})
  end

  def recv_payload_bytes do
    :ets.lookup_element(NetStatsTable, :recv_payload_bytes, 2)
  end

  def add_recv_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :recv_bytes, n, {:k, 0})
  end

  def recv_bytes do
    :ets.lookup_element(NetStatsTable, :recv_bytes, 2)
  end

  def add_recv_ip_overhead_bytes(n) when is_integer(n) and n >= 0 do
    :ets.update_counter(NetStatsTable, :recv_ip_overhead_bytes, n, {:k, 0})
  end

  def recv_ip_overhead_bytes do
    :ets.lookup_element(NetStatsTable, :recv_ip_overhead_bytes, 2)
  end

  def add_recv_tracker_bytes(n) when n >= 0 do
    :ets.update_counter(NetStatsTable, :recv_tracker_bytes, n, {:k, 0})
  end

  def recv_tracker_bytes do
    :ets.lookup_element(NetStatsTable, :recv_tracker_bytes, 2)
  end
end
