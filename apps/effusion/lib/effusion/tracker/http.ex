defmodule Effusion.Tracker.HTTP do
  @callback announce(
    any()
  ) :: {:ok, Effusion.Tracker.Response.t()} | {:error, any()}
end
