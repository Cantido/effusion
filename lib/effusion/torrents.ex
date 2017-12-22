defmodule Effusion.Torrents do
  @moduledoc """
  The directory of running torrents
  """

  @doc """
  Look up a torrent by its info hash. Currently stubbed-out to only recogize
  an all-zeroes info hash.

  ## Examples

      iex> Effusion.Torrents.lookup(<<0 :: 160>>)
      :ok

      iex> Effusion.Torrents.lookup(<<1 :: 160>>)
      :not_found

      iex> Effusion.Torrents.lookup(<<1 :: 170>>)
      :bad_info_hash

  """
  def lookup(info_hash) do
    case info_hash do
      <<0 :: size(160)>> -> :ok
      <<_ :: size(160)>> -> :not_found
      _ -> :bad_info_hash
    end
  end
end
