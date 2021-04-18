defmodule Effusion.DHT.Node do
  @enforce_keys [
    :id,
    :host,
    :port
  ]
  defstruct [
    :id,
    :host,
    :port
  ]

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

  def compact(%__MODULE__{id: id, host: {ip0, ip1, ip2, ip3}, port: port}) do
    id <> <<ip0, ip1, ip2, ip3>> <> <<port::integer-size(16)>>
  end
end
