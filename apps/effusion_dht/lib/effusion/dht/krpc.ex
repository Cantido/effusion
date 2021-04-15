defmodule Effusion.DHT.KRPC do
  alias Effusion.DHT.KRPC.{Query, Response}

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
