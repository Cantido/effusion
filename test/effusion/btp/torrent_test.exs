defmodule Effusion.BTP.TorrentTest do
  use ExUnit.Case
  alias Effusion.Factory

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  test "insert" do
    Factory.insert!(:torrent)
  end
end
