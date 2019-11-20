defmodule Effusion.Mixfile do
  use Mix.Project

  def project do
    [
      app: :effusion,
      version: "0.1.0",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      escript: escript(),
      start_permanent: Mix.env() == :prod,
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      description: "A BitTorrent library.",
      # build with `mix escript.build`
      escript: [main_module: Effusion.CLI],
      package: package(),
      deps: deps(),
      source_url: "https://github.com/Cantido/effusion",
      dialyzer: [flags: [:error_handling, :race_conditions, :underspecs]]
    ]
  end

  defp elixirc_paths(:test), do: ["test/support", "lib"]
  defp elixirc_paths(_), do: ["lib"]

  def application do
    [
      extra_applications: [:logger, :crypto, :ranch, :runtime_tools, :timex],
      mod: {Effusion.Application, []}
    ]
  end

  defp deps do
    [
      {:ex_bencode, "~> 2.0"},
      {:int_set, "~> 1.3"},
      {:httpotion, "~> 3.0.2"},
      {:httpoison, "~> 1.6"},
      {:gen_stage, "~> 0.14"},
      {:gettext, "~> 0.11"},
      {:jason, "~> 1.0"},
      {:logger_file_backend, "~> 0.0.10"},
      {:phoenix, "~> 1.4.11"},
      {:phoenix_pubsub, "~> 1.1"},
      {:plug_cowboy, "~> 2.0"},
      {:ranch, "~> 1.7"},
      {:timex, "~> 3.6"},
      {:tzdata, "~> 0.1.7"},
      {:temp, "~> 0.4", only: :test},
      {:mox, "~> 0.3", only: :test},
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false}
    ]
  end

  defp escript do
    [
      main_module: Effusion.CLI
    ]
  end

  defp package do
    [
      name: :effusion,
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Rosa Richter"],
      licenses: ["GPL v3"],
      links: %{"Github" => "https://github.com/Cantido/effusion"}
    ]
  end
end
