defmodule Effusion.Statistics.Net do
  @moduledoc """
  Tracks network statistics.
  """
  def init do
    :ets.new(NetStatsTable, [:set, :public, :named_table, read_concurrency: false, write_concurrency: true])
    :ets.insert(NetStatsTable, [
      {:sent_payload_bytes, 0},
      {:sent_bytes, 0},
      {:sent_ip_overhead_bytes, 0},
      {:sent_tracker_bytes, 0},
      {:recv_payload_bytes, 0},
      {:recv_bytes, 0},
      {:recv_ip_overhead_bytes, 0},
      {:recv_tracker_bytes, 0},
      {:has_incoming_connections, false}])
  end

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

  def set_has_incoming_connections(n) when is_boolean(n) do
    :ets.insert(NetStatsTable, {:has_incoming_connections, n})
  end

  def has_incoming_connections?() do
    :ets.lookup_element(NetStatsTable, :has_incoming_connections, 2)
  end
end
