defmodule EffusionDesktop.TransfersFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `EffusionDesktop.Transfers` context.
  """

  @doc """
  Generate a transfer.
  """
  def transfer_fixture(attrs \\ %{}) do
    {:ok, transfer} =
      attrs
      |> Enum.into(%{
        info_hash: "some info_hash"
      })
      |> EffusionDesktop.Transfers.create_transfer()

    transfer
  end
end
