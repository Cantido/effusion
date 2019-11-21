defmodule EffusionWeb.Router do
  use EffusionWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api" do
    pipe_through :api

    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: EffusionWeb.Schema

    forward "/", Absinthe.Plug,
      schema: EffusionWeb.Schema
  end
end
