defmodule Effusion.DHT.Node do
  @moduledoc """
  A DHT server in the swarm.
  """

  import Bitwise

  @enforce_keys [:id]
  defstruct [
    id: nil,
    last_response_received_at: :never,
    last_query_received_at: :never
  ]

  @doc """
  Generates twenty-byte node ID.
  """
  def generate_id do
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
