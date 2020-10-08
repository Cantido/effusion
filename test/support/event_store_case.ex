defmodule Effusion.EventStoreCase do
  use ExUnit.CaseTemplate

  setup do
    on_exit(fn ->
      :ok = Application.stop(:effusion)
      :ok = Application.stop(:commanded)
      :ok = Application.stop(:telemetry)

      {:ok, _apps} = Application.ensure_all_started(:effusion)
    end)
  end
end
