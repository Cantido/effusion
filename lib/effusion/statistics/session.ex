defmodule Effusion.Statistics.Session do
  def init do
    :ets.new(SessionStatsTable, [:set, :public, :named_table, read_concurrency: false, write_concurrency: true])
    :ets.insert(SessionStatsTable, [
      {:num_incoming_choke, 0},
      {:num_incoming_unchoke, 0},
      {:num_incoming_interested, 0},
      {:num_incoming_not_interested, 0},
      {:num_incoming_have, 0},
      {:num_incoming_bitfield, 0},
      {:num_incoming_request, 0},
      {:num_incoming_piece, 0},
      {:num_incoming_cancel, 0},
      {:num_incoming_unknown, 0},
      {:num_outgoing_choke, 0},
      {:num_outgoing_unchoke, 0},
      {:num_outgoing_interested, 0},
      {:num_outgoing_not_interested, 0},
      {:num_outgoing_have, 0},
      {:num_outgoing_bitfield, 0},
      {:num_outgoing_request, 0},
      {:num_outgoing_piece, 0},
      {:num_outgoing_cancel, 0},
      {:num_outgoing_unknown, 0}])
  end

  def inc_incoming_message(:choke), do: :ets.update_counter(SessionStatsTable, :num_incoming_choke, 1, {:k, 0})
  def inc_incoming_message(:unchoke), do: :ets.update_counter(SessionStatsTable, :num_incoming_unchoke, 1, {:k, 0})
  def inc_incoming_message(:interested), do: :ets.update_counter(SessionStatsTable, :num_incoming_interested, 1, {:k, 0})
  def inc_incoming_message(:uninterested), do: :ets.update_counter(SessionStatsTable, :num_incoming_not_interested, 1, {:k, 0})
  def inc_incoming_message({:have, _}), do: :ets.update_counter(SessionStatsTable, :num_incoming_have, 1, {:k, 0})
  def inc_incoming_message({:bitfield, _}), do: :ets.update_counter(SessionStatsTable, :num_incoming_bitfield, 1, {:k, 0})
  def inc_incoming_message({:request, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_incoming_request, 1, {:k, 0})
  def inc_incoming_message({:piece, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_incoming_piece, 1, {:k, 0})
  def inc_incoming_message({:cancel, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_incoming_cancel, 1, {:k, 0})
  def inc_incoming_message(_), do: :ets.update_counter(SessionStatsTable, :num_incoming_unknown, 1, {:k, 0})

  def inc_outgoing_message(:choke), do: :ets.update_counter(SessionStatsTable, :num_outgoing_choke, 1, {:k, 0})
  def inc_outgoing_message(:unchoke), do: :ets.update_counter(SessionStatsTable, :num_outgoing_unchoke, 1, {:k, 0})
  def inc_outgoing_message(:interested), do: :ets.update_counter(SessionStatsTable, :num_outgoing_interested, 1, {:k, 0})
  def inc_outgoing_message(:uninterested), do: :ets.update_counter(SessionStatsTable, :num_outgoing_not_interested, 1, {:k, 0})
  def inc_outgoing_message({:have, _}), do: :ets.update_counter(SessionStatsTable, :num_outgoing_have, 1, {:k, 0})
  def inc_outgoing_message({:bitfield, _}), do: :ets.update_counter(SessionStatsTable, :num_outgoing_bitfield, 1, {:k, 0})
  def inc_outgoing_message({:request, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_outgoing_request, 1, {:k, 0})
  def inc_outgoing_message({:piece, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_outgoing_piece, 1, {:k, 0})
  def inc_outgoing_message({:cancel, _, _, _}), do: :ets.update_counter(SessionStatsTable, :num_outgoing_cancel, 1, {:k, 0})
  def inc_outgoing_message(_), do: :ets.update_counter(SessionStatsTable, :num_outgoing_unknown, 1, {:k, 0})

  def num_incoming_choke, do: :ets.lookup_element(SessionStatsTable, :num_incoming_choke, 2)
  def num_incoming_unchoke, do: :ets.lookup_element(SessionStatsTable, :num_incoming_unchoke, 2)
  def num_incoming_interested, do: :ets.lookup_element(SessionStatsTable, :num_incoming_interested, 2)
  def num_incoming_not_interested, do: :ets.lookup_element(SessionStatsTable, :num_incoming_not_interested, 2)
  def num_incoming_have, do: :ets.lookup_element(SessionStatsTable, :num_incoming_have, 2)
  def num_incoming_bitfield, do: :ets.lookup_element(SessionStatsTable, :num_incoming_bitfield, 2)
  def num_incoming_request, do: :ets.lookup_element(SessionStatsTable, :num_incoming_request, 2)
  def num_incoming_piece, do: :ets.lookup_element(SessionStatsTable, :num_incoming_piece, 2)
  def num_incoming_cancel, do: :ets.lookup_element(SessionStatsTable, :num_incoming_cancel, 2)
  def num_incoming_dht_port, do: :ets.lookup_element(SessionStatsTable, :num_incoming_dht_port, 2)
  def num_incoming_suggest, do: :ets.lookup_element(SessionStatsTable, :num_incoming_suggest, 2)
  def num_incoming_have_all, do: :ets.lookup_element(SessionStatsTable, :num_incoming_have_all, 2)
  def num_incoming_have_none, do: :ets.lookup_element(SessionStatsTable, :num_incoming_have_none, 2)
  def num_incoming_reject, do: :ets.lookup_element(SessionStatsTable, :num_incoming_reject, 2)
  def num_incoming_allowed_fast, do: :ets.lookup_element(SessionStatsTable, :num_incoming_allowed_fast, 2)
  def num_incoming_ext_handshake, do: :ets.lookup_element(SessionStatsTable, :num_incoming_ext_handshake, 2)
  def num_incoming_pex, do: :ets.lookup_element(SessionStatsTable, :num_incoming_pex, 2)
  def num_incoming_metadata, do: :ets.lookup_element(SessionStatsTable, :num_incoming_metadata, 2)
  def num_incoming_extended, do: :ets.lookup_element(SessionStatsTable, :num_incoming_extended, 2)
  def num_incoming_unknown, do: :ets.lookup_element(SessionStatsTable, :num_incoming_unknown, 2)

  def num_outgoing_choke, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_choke, 2)
  def num_outgoing_unchoke, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_unchoke, 2)
  def num_outgoing_interested, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_interested, 2)
  def num_outgoing_not_interested, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_not_interested, 2)
  def num_outgoing_have, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_have, 2)
  def num_outgoing_bitfield, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_bitfield, 2)
  def num_outgoing_request, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_request, 2)
  def num_outgoing_piece, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_piece, 2)
  def num_outgoing_cancel, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_cancel, 2)
  def num_outgoing_dht_port, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_dht_port, 2)
  def num_outgoing_suggest, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_suggest, 2)
  def num_outgoing_have_all, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_have_all, 2)
  def num_outgoing_have_none, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_have_none, 2)
  def num_outgoing_reject, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_reject, 2)
  def num_outgoing_allowed_fast, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_allowed_fast, 2)
  def num_outgoing_ext_handshake, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_ext_handshake, 2)
  def num_outgoing_pex, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_pex, 2)
  def num_outgoing_metadata, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_metadata, 2)
  def num_outgoing_extended, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_extended, 2)
  def num_outgoing_unknown, do: :ets.lookup_element(SessionStatsTable, :num_outgoing_unknown, 2)
end
