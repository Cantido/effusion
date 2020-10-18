defmodule Effusion.DHT.EventStoreCase do
  use ExUnit.CaseTemplate

  setup do
    on_exit(fn ->
      :ok = Application.stop(:effusion_dht)
      :ok = Application.stop(:effusion)
      :ok = Application.stop(:commanded)

      {:ok, _apps} = Application.ensure_all_started(:effusion)
      {:ok, _apps} = Application.ensure_all_started(:effusion_dht)
    end)
  end
end
