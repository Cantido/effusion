defmodule Effusion.BTP.TorrentTest do
  use ExUnit.Case, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
    :ok
  end

  test "insert" do
    Effusion.Repo.insert(%Effusion.BTP.Torrent{
      info_hash: "12345678901234567890",
      name: "linuxmint-19.2-cinnamon-64bit.iso"
    })
  end
end
