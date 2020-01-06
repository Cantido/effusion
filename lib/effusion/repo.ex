defmodule Effusion.Repo do
  @moduledoc false
  use Ecto.Repo,
    otp_app: :effusion,
    adapter: Ecto.Adapters.Postgres
end
