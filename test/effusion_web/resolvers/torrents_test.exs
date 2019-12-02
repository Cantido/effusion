defmodule EffusionWeb.Resolvers.TorrentsTest do
  alias EffusionWeb.Resolvers.Torrents
  alias Effusion.Factory
  use ExUnit.Case
  doctest EffusionWeb.Resolvers.Torrents

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  test "all torrents" do
    expected_torrent = Factory.insert!(:torrent)

    {:ok, torrents} = Torrents.all_torrents(nil, nil, nil)

    assert [torrent] = torrents
    assert torrent.id == expected_torrent.id
  end

  test "torrent by info_hash" do
    expected_torrent = Factory.insert!(:torrent)

    encoded_id = expected_torrent.info_hash |> Effusion.Hash.inspect()

    query = %{id: encoded_id}
    {:ok, torrent} = Torrents.find_torrent(nil, query, nil)

    assert torrent.id == expected_torrent.id
  end
end
