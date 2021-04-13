# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :effusion,
  block_size: 16384,
  peer_id: "Effusion Experiment!",
  host: {127, 0, 0, 1},
  port: 8001,
  dht_port: 8006,
  max_requests_per_peer: 200,
  max_half_open_connections: 1000,
  max_peers: 200,
  handshake_timeout: 5_000,
  download_destination: File.cwd!(),
  # dht_node_id should be generated fresh for new installations,
  # by calling Effusion.DHT.node_id() |> Base.encode64()
  dht_node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ=",
  enabled_extensions: [],
  ecto_repos: [Effusion.Repo],
  event_stores: [Effusion.EventStore]

config :commanded, event_store_adapter: Commanded.EventStore.Adapters.EventStore

config :effusion, Effusion.Repo,
  database: "effusion_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost"

config :effusion, Effusion.EventStore,
  serializer: Commanded.Serialization.JsonSerializer,
  database: "event_store_dev",
  username: "postgres",
  password: "postgres",
  hostname: "localhost",
  pool_size: 10

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
