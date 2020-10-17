use Mix.Config

config :commanded, event_store_adapter: Commanded.EventStore.Adapters.InMemory
config :commanded, Commanded.EventStore.Adapters.InMemory, serializer: Commanded.Serialization.JsonSerializer
