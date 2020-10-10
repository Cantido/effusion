defmodule Effusion.Mixfile do
  use Mix.Project

  def project do
    [
      app: :effusion,
      version: "0.2.0",
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
      extra_applications: [:crypto, :logger, :ranch, :runtime_tools, :timex],
      mod: {Effusion.Application, []}
    ]
  end

  defp aliases do
  [
    test: ["ecto.create --quiet", "ecto.migrate", "run priv/repo/seeds.exs", "test"],
    "event_store.reset": ["event_store.drop", "event_store.create", "event_store.init"]
  ]
  end

  defp deps do
    [
      # {:absinthe, "~> 1.4"},
      # {:absinthe_plug, "~> 1.4"},
      # {:absinthe_phoenix, "~> 1.4"},
      {:bento, "~> 0.9"},
      {:broadway, "~> 0.6"},
      {:commanded, "~> 1.2"},
      {:commanded_ecto_projections, "~> 1.2"},
      {:commanded_eventstore_adapter, "~> 1.2"},
      {:cors_plug, "~> 2.0"},
      {:decimal, "~> 1.0"},
      {:ecto_sql, "~> 3.0"},
      {:ecto_network, "~> 1.2.0"},
      {:postgrex, ">= 0.0.0"},
      {:int_set, "~> 1.4"},
      {:httpotion, "~> 3.0.2"},
      {:httpoison, "~> 1.6"},
      {:gen_stage, "~> 1.0"},
      {:gettext, "~> 0.11"},
      {:jason, "~> 1.2"},
      {:logger_file_backend, "~> 0.0.10"},
      {:metatorrent, "~> 1.0"},
      {:phoenix, "~> 1.5"},
      # {:phoenix_pubsub, "~> 2.0", override: true},
      {:plug_cowboy, "~> 2.0"},
      {:ranch, "~> 1.7"},
      {:timex, "~> 3.6"},
      {:tzdata, "~> 0.1.7"},
      {:queutils, "~> 1.2"},
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

  def docs do
    [
      main: "Effusion",
      source_url: "https://github.com/cantido/effusion",
      extras: [
        "README.md"
      ]
    ]
  end
end
