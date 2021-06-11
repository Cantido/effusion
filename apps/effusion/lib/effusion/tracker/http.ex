defmodule Effusion.Tracker.HTTP do
  @callback announce(
    %Effusion.Tracker.Request{}
  ) :: {:ok, Effusion.Tracker.Response.t()} | {:error, any()}
end
