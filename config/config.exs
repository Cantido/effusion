use Mix.Config

config :logger,
  backends: [{LoggerFileBackend, :debug_log}]

config :logger, :debug_log,
  path: "tmp/debug.log",
  level: :debug

config :effusion,
  block_size: 32768,
  peer_id: "Effusion Experiment!",
  max_requests_per_peer: 200,
  max_peers: 20

import_config "#{Mix.env()}.exs"
