alias Effusion.InfoHash

defmodule Effusion.Torrents do
  @moduledoc """
  The directory of running torrents.
  """

  @typedoc """
  Reasons that a torrent lookup might fail.
  """
  @type lookup_failure_reason ::
      :info_hash_not_found
    | :bad_info_hash

  @doc """
  Look up a torrent by its info hash. Currently stubbed-out to only recogize
  an all-zeroes info hash.

  ## Examples

      iex> Effusion.Torrents.lookup(<<0 :: 160>>)
      :ok

      iex> Effusion.Torrents.lookup(<<1 :: 160>>)
      {:error, :info_hash_not_found}

      iex> Effusion.Torrents.lookup(<<1 :: 170>>)
      {:error, :bad_info_hash}

  """
  @spec lookup(InfoHash.t) :: :ok | {:error, lookup_failure_reason}
  def lookup(info_hash) do
    case info_hash do
      <<0 :: size(160)>> -> :ok
      <<_ :: size(160)>> -> {:error, :info_hash_not_found}
      _ -> {:error, :bad_info_hash}
    end
  end
end
