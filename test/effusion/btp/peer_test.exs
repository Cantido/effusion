defmodule Effusion.BTP.PeerTest do
  alias Effusion.Factory
  use ExUnit.Case, async: true
  doctest Effusion.BTP.Peer

  setup do
    Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  test "insert" do
    torrent = Factory.insert! :torrent
    Effusion.Repo.insert(%Effusion.BTP.Peer{
      torrent_id: torrent.id,
      address: %Postgrex.INET{address: {192, 168, 1, 1}},
      port: 8080,
      failcount: 1,
      peer_choking: false,
      peer_interested: true,
      am_choking: false,
      am_interested: true
      })
  end
end
