use Mix.Config

config :effusion,
  thp_client: Effusion.THP.HTTP,
  pwp_client: Effusion.PWP.Peer,
  peer_transport: :gen_tcp
