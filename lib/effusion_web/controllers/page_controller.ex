defmodule EffusionWeb.PageController do
  use EffusionWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
