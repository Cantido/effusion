defmodule Effusion.Math do
  @moduledoc """
  Math functions.
  """

  @doc """
  A shorthand that returns the integer-division result along with the remainder result.

  ## Examples

      iex> Effusion.Math.divrem(5, 2)
      {2, 1}
  """
  def divrem(a, b) when is_integer(a) and is_integer(b) and b != 0 do
    {div(a, b), rem(a, b)}
  end
end
