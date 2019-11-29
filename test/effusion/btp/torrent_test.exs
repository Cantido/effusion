defmodule Effusion.BTP.TorrentTest do
  use ExUnit.Case, async: true
  alias Effusion.Factory

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
    :ok
  end

  test "insert" do
    Factory.insert!(:torrent)
  end
end
