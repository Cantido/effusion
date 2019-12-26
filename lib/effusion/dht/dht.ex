defmodule Effusion.DHT do
  @moduledoc """
  Documentation for Effusion.DHT.
  """

  defguard is_node_id(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_info_hash(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_inet_port(n) when is_integer(n) and n in 1..65535

  def node_id do
    :crypto.strong_rand_bytes(20)
  end

  def transaction_id do
    Base.encode64(:crypto.strong_rand_bytes(1), padding: false)
  end

  def token do
    Base.encode64(:crypto.strong_rand_bytes(4))
  end
end
