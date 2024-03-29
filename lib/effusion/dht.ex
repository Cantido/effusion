defmodule Effusion.DHT do
  @moduledoc """
  Documentation for Effusion.DHT.
  """

  defguard is_node_id(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_info_hash(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_inet_port(n) when is_integer(n) and n in 1..65_535

  @doc """
  Generates a token for use in `get_peers` responses.
  """
  def generate_announce_peer_token do
    Base.encode64(:crypto.strong_rand_bytes(6))
  end

  def local_node_id do
    Application.fetch_env!(:effusion, :node_id) |> Base.decode64!()
  end
end
