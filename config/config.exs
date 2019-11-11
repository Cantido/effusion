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

import_config "#{Mix.env()}.exs"
