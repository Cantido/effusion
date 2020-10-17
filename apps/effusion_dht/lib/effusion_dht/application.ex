defmodule Effusion.DHT.Application do
  alias Effusion.DHT.Router
  use Commanded.Application,
    otp_app: :effusion_dht,
    event_store: [
      adapter: Application.fetch_env!(:commanded, :event_store_adapter),
      event_store: Effusion.CQRS.EventStore
    ]

  router Router
end
