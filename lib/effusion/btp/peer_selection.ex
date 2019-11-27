defmodule Effusion.BTP.PeerSelection do
  alias Effusion.BTP.Peer
  alias Effusion.Repo
  import Ecto.Query
  @moduledoc """
  Selects which peers to connect to.
  """

  def select_lowest_failcount(_peers, count) when is_integer(count) and count >= 0 do
    Peer
    |> order_by([desc: :failcount])
    |> limit(^count)
    |> Repo.all()
  end
end
