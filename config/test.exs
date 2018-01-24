use Mix.Config

config :effusion,
  thp_client: Effusion.THP.Mock,
  pwp_client: Effusion.PWP.Mock,
  peer_transport: Effusion.Transport.Mock
