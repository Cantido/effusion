use Mix.Config

config :logger,
  backends: [{LoggerFileBackend, :debug_log}]

config :logger, :debug_log,
  path: "tmp/debug.log",
  level: :debug

config :effusion,
  server_host: {127, 0, 0, 1},
  server_port: 4001,
  server_address: {{127, 0, 0, 1}, 4001}
