defmodule Effusion.Commanded do
  use Commanded.Application,
    otp_app: :effusion,
    event_store: [
      adapter: Application.fetch_env!(:commanded, :event_store_adapter),
      event_store: Effusion.EventStore
    ]

  router(Effusion.Downloads.Router)
  router(Effusion.PWP.Router)
end
