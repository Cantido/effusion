import Config

config :effusion,
  peer_id: "Effusion Experiment!",
  host: {127, 0, 0, 1},
  port: 8001,
  max_requests_per_peer: 200,
  max_half_open_connections: 1000,
  max_peers: 200,
  handshake_timeout: 5_000,
  download_destination: File.cwd!(),
  enabled_extensions: [],
  tracker_worker: Effusion.Tracker.Finch

config :effusion_dht,
  port: 8006,
  # dht_node_id should be generated fresh for new installations,
  # by calling Effusion.DHT.generate_node_id() |> Base.encode64()
  node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ=",
  ecto_repos: [Effusion.Repo],
  event_stores: [Effusion.EventStore]

import_config "#{Mix.env()}.exs"
