defmodule Effusion.CQRS.Application do
  alias Effusion.CQRS.Router
  use Commanded.Application,
    otp_app: :effusion,
    event_store: [
      adapter: Application.fetch_env!(:commanded, :event_store_adapter),
      event_store: Effusion.CQRS.EventStore
    ]

  router Router
  router Effusion.PWP.Router
end
