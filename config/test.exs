use Mix.Config

config :logger, level: :debug

config :effusion,
  thp_client: Effusion.THP.Mock
