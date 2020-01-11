defmodule Effusion.Hash do
  @moduledoc """
  Utilities for working with SHA-1 hashes.
  """

  @doc """
  Returns `true` if `term` is a 20-byte binary; otherwise returns `false`.

  Allowed in guard tests.

  ## Examples

      iex> Effusion.Hash.is_hash("12345678901234567890")
      true

      iex> Effusion.Hash.is_hash("1234567890")
      false
  """
  defguard is_hash(term) when is_binary(term) and byte_size(term) == 20

  @doc """
  Formats a hash into a nice, readable hex string.

  ## Examples

      iex> Effusion.Hash.calc("Hello!") |> Effusion.Hash.encode()
      "69342c5c39e5ae5f0077aecc32c0f81811fb8193"
  """
  def encode(hash) when is_hash(hash) do
    Base.encode16(hash, case: :lower)
  end

  def decode(base16) when is_binary(base16) and byte_size(base16) == 40 do
    Base.decode16!(base16, case: :mixed)
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
end
