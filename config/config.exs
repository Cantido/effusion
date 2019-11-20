# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :logger,
  backends: [{LoggerFileBackend, :debug_log}]

config :logger, :debug_log,
  path: "tmp/debug.log",
  level: :debug

config :effusion,
  block_size: 16384,
  peer_id: "Effusion Experiment!",
  max_requests_per_peer: 100,
  max_peers: 100

# Configures the endpoint
config :effusion, EffusionWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "MthtN4P5iyPai9u+Unl04vOhNnflrEL8haKx+g4Sik2SZqWIy7nM9U1RnQjUpyx/",
  render_errors: [view: EffusionWeb.ErrorView, accepts: ~w(json)],
  pubsub: [name: Effusion.PubSub, adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
