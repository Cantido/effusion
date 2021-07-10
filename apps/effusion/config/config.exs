# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :effusion,
  peer_id: "Effusion Experiment!",
  host: {127, 0, 0, 1},
  port: 8001,
  max_requests_per_peer: 200,
  max_half_open_connections: 1000,
  max_peers: 200,
  handshake_timeout: 5_000,
  download_destination: File.cwd!(),
  enabled_extensions: [],
  tracker_worker: Effusion.Tracker.Finch

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
