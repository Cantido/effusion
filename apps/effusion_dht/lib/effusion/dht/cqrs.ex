defmodule Effusion.DHT.Commanded do
  use Commanded.Application,
    otp_app: :effusion_dht,
    event_store: [
      adapter: Application.fetch_env!(:commanded, :event_store_adapter),
      event_store: Effusion.EventStore
    ]

  router Effusion.DHT.Router
end
