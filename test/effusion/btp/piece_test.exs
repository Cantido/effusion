defmodule Effusion.BTP.PieceTest do
  alias Effusion.BTP.Piece
  alias Effusion.Factory
  alias Effusion.Repo
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
    :ok
  end

  test "insert" do
    piece = Factory.insert!(:piece)
    Repo.get(Piece, piece.id)
  end
end
