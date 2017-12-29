defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion

  test "starts a torrent registry" do
    info_hash = <<1 :: size(160)>>

    Registry.register(Effusion.TorrentRegistry, info_hash, :ok)
    assert [{self(), :ok}] == Registry.lookup(Effusion.TorrentRegistry, info_hash)
  end
end
