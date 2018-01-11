defmodule Effusion.Mixfile do
  use Mix.Project

  def project do
    [
      app: :effusion,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      description: "A BitTorrent library.",
      package: package(),
      deps: deps(),
      source_url: "https://github.com/Cantido/effusion",
      dialyzer: [ flags: ["-Wunmatched_returns", :error_handling, :race_conditions, :underspecs]]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Effusion.Application, []}
    ]
  end

  defp deps do
    [
      {:ex_bencode, "~> 1.1.0"},
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false}
    ]
  end

  defp package do
    [ name: :effusion,
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      maintainers: ["Rosa Richter"],
      licenses: ["GPL v3"],
      links: %{"Github" => "https://github.com/Cantido/effusion"},
    ]
  end
end
