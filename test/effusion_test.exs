defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion

  test "greets the world" do
    assert Effusion.hello() == :world
  end
end
