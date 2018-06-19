defmodule Effusion.Hash do
  @moduledoc """
  Utilities for working with SHA-1 hashes.
  """

  @doc """
  Formats a hash into a nice, readable hex string.

  ## Examples

      iex> Effusion.Hash.inspect(<<1, 2, 3>>)
      "010203"

      iex> Effusion.Hash.inspect(<<50, 100, 150, 200, 250>>)
      "326496c8fa"
  """
  def inspect(hash) when is_binary(hash) do
    Base.encode16 hash, case: :lower
  end


  @doc """
  Calculate the SHA-1 hash on a binary.

  ## Examples

      iex> Effusion.Hash.calc("Hello!")
      <<105, 52, 44, 92, 57, 229, 174, 95, 0, 119, 174, 204, 50, 192, 248, 24, 17, 251, 129, 147>>
  """
  def calc(bin) when is_binary(bin) do
    :crypto.hash(:sha, bin)
  end

  @doc """
  Check a piece of data's SHA-1 hash against another SHA-1 hash.

  ## Examples

      Effusion.Hash.matches(Effusion.Hash.calc("Hello!"), "Hello!")
      true

      Effusion.Hash.matches(<<1, 2, 3>>, "Nope!")
      false
  """
  def matches?(hash, data) when is_binary(hash) do
    calc(data) == hash
  end
end
