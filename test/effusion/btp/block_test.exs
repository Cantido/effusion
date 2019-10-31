defmodule Effusion.BTP.BlockTest do
  use ExUnit.Case, async: true
  doctest Effusion.BTP.Block
  alias Effusion.BTP.Block

  test "sequential blocks" do
    assert Block.sequential?(Block.new(0, 0, "h"), Block.new(0, 1, "i"))

    refute Block.sequential?(Block.new(0, 1, "i"), Block.new(0, 0, "h"))

    refute Block.sequential?(Block.new(0, 0, "a"), Block.new(1, 1, "d"))
  end

  test "adjacent blocks" do
    assert Block.adjacent?(%{index: 0, offset: 0, data: "h"}, %{index: 0, offset: 1, data: "i"})

    assert Block.adjacent?(%{index: 0, offset: 1, data: "i"}, %{index: 0, offset: 0, data: "h"})

    refute Block.adjacent?(%{index: 0, offset: 0, data: "a"}, %{index: 0, offset: 2, data: "c"})
  end

  test "adjacent blocks can be merged" do
    merged1 = Block.merge(%{index: 0, offset: 0, data: "h"}, %{index: 0, offset: 1, data: "i"})

    merged2 = Block.merge(%{index: 0, offset: 0, data: "h"}, %{index: 0, offset: 1, data: "i"})

    assert merged1 == %Block{index: 0, offset: 0, data: "hi", size: 2}
    assert merged2 == %Block{index: 0, offset: 0, data: "hi", size: 2}
  end

  test "knows its size if it contains data" do
    assert 1 == Block.size(%{index: 0, offset: 0, data: "h"})
  end

  test "knows its size if it IDs a block" do
    assert 1 == Block.size(%{index: 0, offset: 0, size: 1})
  end

  test "can be split into uneven pieces" do
    unsplit = Block.id(0, 0, 5)
    splits = Block.split(unsplit, 2)

    assert Enum.count(splits) == 3
    assert splits == MapSet.new([Block.id(0, 0, 2), Block.id(0, 2, 2), Block.id(0, 4, 1)])
  end

  test "can be split into even pieces" do
    unsplit = Block.id(0, 0, 6)
    splits = Block.split(unsplit, 2)

    assert Enum.count(splits) == 3
    assert splits == MapSet.new([Block.id(0, 0, 2), Block.id(0, 2, 2), Block.id(0, 4, 2)])
  end
end
