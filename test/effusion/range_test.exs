defmodule Effusion.RangeTest do
  use ExUnit.Case
  doctest Effusion.Range

  test "alternates of doctests" do
    assert Effusion.Range.overlap(2..7, 0..5) == 2..5
    assert Effusion.Range.overlap(1..10, 2..3) == 2..3
    assert Effusion.Range.overlap(0..3, 6..9) == nil
  end
end
