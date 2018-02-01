defmodule Effusion.IntSetTest do
  use ExUnit.Case
  alias Effusion.IntSet
  doctest Effusion.IntSet

  test "member? returns false if IntSet is too small to contain x" do
    set = IntSet.new(<<0b0000_0000>>)

    refute IntSet.member?(set, 8)
  end

  test "put can insert values smaller than the max" do
    set = IntSet.new(<<0b0000_0001>>)
       |> IntSet.put(4)

    assert IntSet.member?(set, 4)
  end

  test "put can insert a value one larger than the max" do
    set = IntSet.new(<<0 :: 1>>)
       |> IntSet.put(1)

    assert IntSet.member?(set, 1)
  end

  test "put can expand the bitstring" do
    set = IntSet.new()
       |> IntSet.put(7)

    assert IntSet.member?(set, 7)
  end
end
