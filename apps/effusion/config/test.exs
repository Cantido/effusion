use Mix.Config

config :logger, level: :info

config :effusion,
  thp_client: Effusion.THP.Mock
