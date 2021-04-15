defmodule Effusion.DHT.Transaction do
  @moduledoc """
  A correlated request & response.
  """

  @doc """
  Generates a unique ID for transactions.
  """
  def generate_id do
    Base.encode64(:crypto.strong_rand_bytes(1), padding: false)
  end
end
