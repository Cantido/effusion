use Mix.Config

config :logger, level: :debug

config :effusion,
  thp_client: Effusion.THP.HTTP,
  pwp_client: Effusion.PWP.Connection,
  server_host: {127, 0, 0, 1},
  server_port: 4001,
  server_address: {{127, 0, 0, 1}, 4001}
