use Mix.Config

config :effusion_dht,
  port: 8006,
  node_id: "zjuXKldLo4rJMGR1Ww/ykZFlXLQ=",
  ecto_repos: [Effusion.Repo],
  event_stores: [Effusion.CQRS.EventStore]

config :commanded, event_store_adapter: Commanded.EventStore.Adapters.EventStore

import_config "#{Mix.env()}.exs"
