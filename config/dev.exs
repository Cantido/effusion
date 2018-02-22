use Mix.Config

config :effusion,
  thp_client: Effusion.THP.HTTP,
  pwp_client: Effusion.PWP.Peer,
  peer_transport: :gen_tcp,
  server_host: {127, 0, 0, 1},
  server_port: 4001,
  server_address: {{127, 0, 0, 1}, 4001}
