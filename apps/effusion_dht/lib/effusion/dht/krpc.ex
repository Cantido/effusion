defmodule Effusion.DHT.KRPC do
  def new_query(txid, method_name, arguments) do
    %{
      t: txid,
      y: "q",
      q: method_name,
      a: arguments
    }
  end

  def new_response(txid, return_values) do
    %{
      t: txid,
      y: "r",
      r: return_values
    }
  end

  def new_error(txid, error_message) do
    %{
      t: txid,
      y: "e",
      e: error_message
    }
  end

  @doc """
  Generates a unique ID for transactions.
  """
  def generate_transaction_id do
    Base.encode64(:crypto.strong_rand_bytes(1), padding: false)
  end

  def encode!(message) do
    Bento.encode!(message)
  end

  def decode!(message) do
    Bento.decode!(message)
  end
end
