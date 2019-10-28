use Mix.Config

config :logger, level: :error

config :effusion,
  block_size: 16384,
  peer_id: "Effusion Experiment!"

import_config "#{Mix.env()}.exs"
