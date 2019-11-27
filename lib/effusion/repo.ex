defmodule Effusion.Repo do
  use Ecto.Repo,
    otp_app: :effusion,
    adapter: Ecto.Adapters.Postgres
end
