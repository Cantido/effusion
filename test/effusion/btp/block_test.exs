defmodule Effusion.BTP.BlockTest do
  use ExUnit.Case
  alias Effusion.BTP.Block
  alias Effusion.Repo
  alias Effusion.Factory
  doctest Effusion.BTP.Block

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, {:shared, self()})
    :ok
  end

  test "insert" do
    block = Factory.insert!(:block)

    Repo.get(Block, block.id)
  end

end
