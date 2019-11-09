use Mix.Config

config :logger,
  backends: [{LoggerFileBackend, :debug_log}]

config :logger, :debug_log,
  path: "tmp/debug.log",
  level: :debug

config :effusion,
  block_size: 32768,
  peer_id: "Effusion Experiment!"

import_config "#{Mix.env()}.exs"
