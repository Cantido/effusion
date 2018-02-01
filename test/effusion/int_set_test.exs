defmodule Effusion.IntSetTest do
  use ExUnit.Case
  doctest Effusion.IntSet

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
end
