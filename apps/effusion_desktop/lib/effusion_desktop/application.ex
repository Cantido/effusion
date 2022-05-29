defmodule EffusionDesktop.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      EffusionDesktopWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: EffusionDesktop.PubSub},
      # Start the Endpoint (http/https)
      EffusionDesktopWeb.Endpoint,
      # Start a worker by calling: EffusionDesktop.Worker.start_link(arg)
      # {EffusionDesktop.Worker, arg}
      {Desktop.Window, [app: :effusion_desktop, id: EffusionDesktopWindow, url: &EffusionDesktopWeb.Endpoint.url/0]}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: EffusionDesktop.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    EffusionDesktopWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  @impl true
  def stop(_state) do
    # Do a hard shutdown after the application has been stopped.
    #
    # Another, perhaps better, option is `System.stop/0`, but this results in a
    # rather annoying lag when quitting the terminal application.
    System.halt()
  end
end
