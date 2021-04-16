defmodule Effusion.DHT do
  @moduledoc """
  Documentation for Effusion.DHT.
  """

  import Bitwise

  defguard is_node_id(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_info_hash(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_inet_port(n) when is_integer(n) and n in 1..65_535

  @doc """
  Generates a token for use in `get_peers` responses.
  """
  def token do
    Base.encode64(:crypto.strong_rand_bytes(6))
  end

  def local_node_id do
    Application.fetch_env!(:effusion_dht, :node_id) |> Base.decode64!()
  end

  @doc """
  Generates twenty-byte node ID.
  """
  def generate_node_id do
    :crypto.strong_rand_bytes(20)
  end

  @doc """
    Calcuates the distance between two node IDs.
    Smaller values mean the two nodes are closer.

    This function uses the Kademlia distance metric: XOR.

    ## Examples

        iex> Effusion.DHT.distance("12345678901234567890", "12345678901234567890")
        0

        iex> Effusion.DHT.distance("12345678901234567890", "09876543210987654321")
        5955258228003349104393039705260020053666630401
  """
  def distance(<<a::160>>, <<b::160>>) do
    bxor(a, b)
  end
end
