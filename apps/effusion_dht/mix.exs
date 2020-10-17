defmodule EffusionDht.MixProject do
  use Mix.Project

  def project do
    [
      app: :effusion_dht,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:effusion, in_umbrella: true},
      {:bento, "~> 0.9"},
      {:commanded, "~> 1.2"},
      {:commanded_ecto_projections, "~> 1.2"}
    ]
  end
end
