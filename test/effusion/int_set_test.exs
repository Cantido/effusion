defmodule Effusion.IntSetTest do
  use ExUnit.Case
  alias Effusion.IntSet
  doctest Effusion.IntSet

  test "is collectable" do
    collected = Enum.into([1, 2, 3], IntSet.new())

    assert Enum.member?(collected, 1)
    assert Enum.member?(collected, 2)
    assert Enum.member?(collected, 3)
  end

  test "is countable as enumerable" do
    assert 0 = IntSet.new() |> Enum.count()
    assert 1 = IntSet.new([1]) |> Enum.count()
  end

  test "can check members enumerable" do
    assert IntSet.new([1]) |> Enum.member?(1)
    refute IntSet.new() |> Enum.member?(1)
  end

  test "can map a function as an enumerable" do
    incremented = IntSet.new([1, 2, 3]) |> Enum.map(&(&1 + 1))
    assert 2 in incremented
    assert 3 in incremented
    assert 4 in incremented
  end

  test "member? returns false if IntSet is too small to contain x" do
    set = IntSet.new(<<0b0000_0000>>)

    refute Enum.member?(set, 8)
  end

  test "put can insert values smaller than the max" do
    set = IntSet.new(<<0b0000_0001>>)
       |> IntSet.put(4)

    assert Enum.member?(set, 4)
  end

  test "put can insert a value one larger than the max" do
    set = IntSet.new(<<0 :: 1>>)
       |> IntSet.put(1)

    assert Enum.member?(set, 1)
  end

  test "put can expand the bitstring" do
    set = IntSet.new()
       |> IntSet.put(7)

    assert Enum.member?(set, 7)
  end

  test "inspectable" do
    set = IntSet.new([1, 2, 3])

    assert inspect(set) == "#IntSet<[1, 2, 3]>"
  end
end
