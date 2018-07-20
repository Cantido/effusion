use Mix.Config

config :effusion, block_size: 16384

import_config "#{Mix.env()}.exs"
