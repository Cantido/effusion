defmodule Effusion.DHT do
  @moduledoc """
  Documentation for Effusion.DHT.
  """

  alias Effusion.DHT.KRPC.{Query, Response}

  defguard is_node_id(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_info_hash(binary) when is_binary(binary) and byte_size(binary) == 20
  defguard is_inet_port(n) when is_integer(n) and n in 1..65_535

  @doc """
  Generates a token for use in `get_peers` responses.
  """
  def token do
    Base.encode64(:crypto.strong_rand_bytes(6))
  end

  def send_response(response, host, port) do
    Response.encode(response)
    |> Bento.encode!()
    |> send(host, port)
  end

  def send_query(query, host, port) do
    Query.encode(query)
    |> Bento.encode!()
    |> send(host, port)
  end

  defp send(message, host, port) do
    with {:ok, host} <- :inet.parse_address(String.to_charlist(host)),
         {:ok, socket} <- :gen_udp.open(0),
         :ok <- :gen_udp.send(socket, host, port, message),
         :ok <- :gen_udp.close(socket) do
      :ok
    end
  end
end
