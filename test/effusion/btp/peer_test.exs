defmodule Effusion.BTP.PeerTest do
  use ExUnit.Case, async: true
  doctest Effusion.BTP.Peer

  setup do
    Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  test "insert" do
    Effusion.Repo.insert(%Effusion.BTP.Peer{
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
