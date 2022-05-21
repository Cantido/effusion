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
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp elixirc_paths(:test), do: ["test/support", "lib"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Effusion.DHT.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:effusion, in_umbrella: true},
      {:bento, "~> 0.9"},
      {:solvent, github: "Cantido/solvent"},
      {:temp, "~> 0.4", only: :test},
      {:mox, "~> 1.0", only: :test}
    ]
  end


  def aliases do
    [
      setup: ["deps.get"],
    ]
  end
end
