defmodule Effusion.BTP.BlockTest do
  use ExUnit.Case
  doctest Effusion.BTP.Block
  alias Effusion.BTP.Block

  test "sequential blocks" do
    assert Block.sequential?(
      Block.new(0, 0, "h"),
      Block.new(0, 1, "i"))

    refute Block.sequential?(
      Block.new(0, 1, "i"),
      Block.new(0, 0, "h"))
  end

  test "adjacent blocks" do
    assert Block.adjacent?(
      %{index: 0, offset: 0, data: "h"},
      %{index: 0, offset: 1, data: "i"})

    assert Block.adjacent?(
      %{index: 0, offset: 1, data: "i"},
      %{index: 0, offset: 0, data: "h"})
  end

  test "adjacent blocks can be merged" do
    merged1 = Block.merge(
      %{index: 0, offset: 0, data: "h"},
      %{index: 0, offset: 1, data: "i"})

    merged2 = Block.merge(
      %{index: 0, offset: 0, data: "h"},
      %{index: 0, offset: 1, data: "i"})

    assert merged1 == %{index: 0, offset: 0, data: "hi"}
    assert merged2 == %{index: 0, offset: 0, data: "hi"}
  end

  test "knows its size if it contains data" do
    assert 1 == Block.size(%{index: 0, offset: 0, data: "h"})
  end

  test "knows its size if it IDs a block" do
    assert 1 == Block.size(%{index: 0, offset: 0, size: 1})
  end
end
