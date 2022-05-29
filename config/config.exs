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
  tracker_worker: Effusion.Tracker.HTTPoison

config :effusion_dht,
  port: 8006,
  # dht_node_id should be generated fresh for new installations,
  # by calling Effusion.DHT.generate_node_id() |> Base.encode64()
  node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ=",
  ecto_repos: [Effusion.Repo],
  event_stores: [Effusion.EventStore]

config :logger, :console,
  metadata: [
    :info_hash,
    :peer_id,
    :event_type,
    :request_id
  ]

config :effusion_desktop, EffusionDesktopWeb.Endpoint,
  url: [host: "localhost"],
  render_errors: [view: EffusionDesktopWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: EffusionDesktop.PubSub,
  live_view: [signing_salt: "Cna4QSzc"]

# Configure esbuild (the version is required)
config :esbuild,
  version: "0.14.29",
  default: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../apps/effusion_desktop/assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :phoenix, :json_library, Jason

import_config "#{config_env()}.exs"
