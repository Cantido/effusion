use Mix.Config

config :effusion_dht,
  port: 8006,
  # dht_node_id should be generated fresh for new installations,
  # by calling Effusion.DHT.generate_node_id() |> Base.encode64()
  node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ=",
  ecto_repos: [Effusion.Repo],
  event_stores: [Effusion.EventStore]

config :commanded, event_store_adapter: Commanded.EventStore.Adapters.EventStore

import_config "#{Mix.env()}.exs"
