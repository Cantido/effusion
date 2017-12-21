defmodule Effusion.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
