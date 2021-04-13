use Mix.Config

config :logger, level: :debug

config :effusion,
  thp_client: Effusion.THP.Mock

config :effusion, Effusion.Repo,
  pool: Ecto.Adapters.SQL.Sandbox,
  database: "effusion_test",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  log: false

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :effusion, EffusionWeb.Endpoint,
  http: [port: 4002],
  server: false

config :commanded, event_store_adapter: Commanded.EventStore.Adapters.InMemory

config :commanded, Commanded.EventStore.Adapters.InMemory,
  serializer: Commanded.Serialization.JsonSerializer
