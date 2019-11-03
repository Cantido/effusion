use Mix.Config

config :logger,
  backends: [{LoggerFileBackend, :debug_log}]

config :logger, :debug_log,
  path: "tmp/debug.log",
  level: :debug,
  metadata: :all

config :effusion,
  block_size: 16384,
  peer_id: "Effusion Experiment!"

import_config "#{Mix.env()}.exs"
