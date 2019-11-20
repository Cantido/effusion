defmodule EffusionWeb.Router do
  use EffusionWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", EffusionWeb do
    pipe_through :api
  end
end
