use Mix.Config

config :logger, level: :debug

config :effusion,
  thp_client: Effusion.THP.Mock

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :effusion, EffusionWeb.Endpoint,
  http: [port: 4002],
  server: false
