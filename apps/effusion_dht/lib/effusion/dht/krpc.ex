defmodule Effusion.DHT.KRPC do
  alias Effusion.DHT.KRPC.Message

  def new_query(txid, method_name, arguments) do
    %Message{
      t: txid,
      y: "q"
      q: method_name,
      a: arguments
    }
  end

  def new_response(txid, return_values) do
    %Message{
      t: txid,
      y: "q",
      r: return_values
    }
  end

  def new_error(txid, error_message) do
    %Message{
      t: txid,
      y: "e",
      e: error_message
    }
  end

  def encode!(message) do
    message
    |> Map.from_struct()
    |> Bento.encode!()
  end

  def decode!(message) do
    m = Bento.decode!(message)

    %Message{
      t: m["t"],
      y: m["y"],
      v: m["v"],
      q: m["q"],
      a: m["a"],
      r: m["r"],
      e: m["e"]
    }
  end

  @doc """
  Send a KRPC message
  """
  def send_message(message, host, port) when is_map(message) do
    with {:ok, dict} <- Message.encode(),
         {:ok, host} <- :inet.parse_address(String.to_charlist(host)),
         {:ok, socket} <- :gen_udp.open(0),
         :ok <- :gen_udp.send(socket, host, port, dict),
         :ok <- :gen_udp.close(socket) do
      :ok
    end
  end
end
