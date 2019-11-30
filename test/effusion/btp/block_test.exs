defmodule Effusion.BTP.BlockTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Block
  alias Effusion.Repo
  alias Effusion.Factory
  doctest Effusion.BTP.Block

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    :ok
  end

  test "insert" do
    block = Factory.insert!(:block)

    Repo.get(Block, block.id)
  end

end
