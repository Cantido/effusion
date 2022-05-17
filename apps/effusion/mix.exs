defmodule Effusion.Mixfile do
  use Mix.Project

  def project do
    [
      app: :effusion,
      version: "0.2.0",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      start_permanent: Mix.env() == :prod,
      description: "A BitTorrent library.",
      package: package(),
      deps: deps(),
      docs: docs(),
      aliases: aliases(),
      source_url: "https://github.com/Cantido/effusion",
      dialyzer: [flags: [:error_handling, :race_conditions, :underspecs]]
    ]
  end

  defp elixirc_paths(:test), do: ["test/support", "lib"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      extra_applications: [:crypto, :logger, :ranch, :runtime_tools],
      mod: {Effusion.Application, []}
    ]
  end

  defp deps do
    [
      {:bento, "~> 0.9"},
      {:bypass, "~> 2.1", only: :test},
      {:finch, "~> 0.12.0"},
      {:honeydew, "~> 1.5"},
      {:int_set, "~> 1.5"},
      {:logger_file_backend, "~> 0.0.10"},
      {:metatorrent, "~> 1.0"},
      {:mox, "~> 1.0", only: :test},
      {:ranch, "~> 1.7"},
      {:temp, "~> 0.4", only: :test},
      {:solvent, github: "Cantido/solvent"},
      {:credo, "~> 1.5", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
      {:dialyxir, "~> 1.1", only: [:dev], runtime: false},
    ]
  end

  defp package do
    [
      name: :effusion,
      files: ["lib", "mix.exs"],
      maintainers: ["Rosa Richter"],
      licenses: ["GPL v3"],
      links: %{"Github" => "https://github.com/Cantido/effusion"}
    ]
  end

  def docs do
    [
      main: "Effusion",
      source_url: "https://github.com/cantido/effusion"
    ]
  end

  def aliases do
    [
      setup: ["deps.get"],
    ]
  end
end
