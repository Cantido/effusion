defmodule EffusionUmbrella.Mixfile do
  use Mix.Project

  def project do
    [
      version: "0.1.0",
      apps_path: "apps",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      releases: [
        effusion: [
          applications: [
            effusion: :permanent,
            effusion_cli: :permanent,
            effusion_dht: :permanent,
            effusion_desktop: :permanent
          ]
        ]
      ],
      deps: deps(),
      aliases: aliases()
    ]
  end

  defp deps do
    []
  end

  defp aliases do
    [
      setup: ["cmd mix setup"]
    ]
  end
end
