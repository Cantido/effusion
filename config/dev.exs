use Mix.Config

config :effusion,
  thp_client: Effusion.THP.HTTP,
  server_host: {127, 0, 0, 1},
  server_port: 4001,
  server_address: {{127, 0, 0, 1}, 4001}
