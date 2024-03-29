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
  tracker_worker: Effusion.Tracker.HTTPoison,
  dht_port: 8006,
  # dht_node_id should be generated fresh for new installations,
  # by calling Effusion.DHT.generate_node_id() |> Base.encode64()
  node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ="

config :logger, :console,
  metadata: [
    :info_hash,
    :peer_id,
    :event_type,
    :request_id
  ]

import_config "#{config_env()}.exs"
