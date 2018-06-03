use Mix.Config

config :effusion,
  thp_client: Effusion.THP.HTTP,
  pwp_client: Effusion.Application.PeerServer,
  peer_transport: :gen_tcp
