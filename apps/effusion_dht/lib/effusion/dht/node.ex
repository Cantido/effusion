defmodule Effusion.DHT.Node do
  import Bitwise

  @fifteen_minutes_in_seconds 15 * 60

  @enforce_keys [
    :id,
    :host,
    :port
  ]
  defstruct [
    id: nil,
    host: nil,
    port: nil,
    last_query_received_at: nil,
    last_response_received_at: nil,
    consecutive_failed_queries: 0
  ]

  def new(id, host, port) do
    %__MODULE__{
      id: id,
      host: host,
      port: port
    }
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

        iex> Effusion.DHT.Node.distance("12345678901234567890", "12345678901234567890")
        0

        iex> Effusion.DHT.Node.distance("12345678901234567890", "09876543210987654321")
        5955258228003349104393039705260020053666630401
  """
  def distance(<<a::160>>, <<b::160>>) do
    bxor(a, b)
  end

  def compact(%__MODULE__{id: id, host: {ip0, ip1, ip2, ip3}, port: port}) do
    id <> <<ip0, ip1, ip2, ip3>> <> <<port::integer-size(16)>>
  end

  def query_received_at(node = %__MODULE__{last_query_received_at: prev_timestamp}, timestamp) do
    if is_nil(prev_timestamp) or DateTime.compare(prev_timestamp, timestamp) == :lt do
      %__MODULE__{node | last_query_received_at: timestamp}
    else
      node
    end
  end

  def response_received_at(node = %__MODULE__{last_response_received_at: prev_timestamp}, timestamp) do
    if is_nil(prev_timestamp) or DateTime.compare(prev_timestamp, timestamp) == :lt do
      %__MODULE__{node |
        last_response_received_at: timestamp,
        consecutive_failed_queries: 0
      }
    else
      node
    end
  end

  def failed_query(node = %__MODULE__{consecutive_failed_queries: prev_count}) do
    %__MODULE__{node | consecutive_failed_queries: prev_count + 1}
  end

  def status(node, now) do
    cond do
      node.consecutive_failed_queries >= 5 ->
        :bad
      ever_responded?(node) and time_since_last_response(node, now, :second) <= @fifteen_minutes_in_seconds ->
        :good
      ever_responded?(node) and ever_queried?(node) and time_since_last_query(node, now, :second) <= @fifteen_minutes_in_seconds ->
        :good
      ever_responded?(node) and time_since_last_response(node, now, :second) > @fifteen_minutes_in_seconds ->
        :questionable
      ever_queried?(node) and time_since_last_query(node, now, :second) > @fifteen_minutes_in_seconds ->
        :questionable
      true ->
        :unknown
    end
  end

  def ever_responded?(node) do
    not is_nil(node.last_response_received_at)
  end

  def ever_queried?(node) do
    not is_nil(node.last_query_received_at)
  end

  def time_since_last_response(node, now, time_unit) do
    DateTime.diff(now, node.last_response_received_at, time_unit)
  end

  def time_since_last_query(node, now, time_unit) do
    DateTime.diff(now, node.last_query_received_at, time_unit)
  end
end
