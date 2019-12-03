defmodule Effusion.BTP.RequestTest do
  alias Effusion.Factory
  use ExUnit.Case, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end


  test "insert" do
    torrent = Factory.insert!(:torrent)

    {:ok, piece} = Effusion.Repo.insert(%Effusion.BTP.Piece{
      torrent: torrent,
      index: 0,
      hash: "12345678901234567890",
      size: 1_000_000
    })

    {:ok, block} = Effusion.Repo.insert(%Effusion.BTP.Block{
      piece: piece,
      offset: 0,
      data: "tiny\n",
      size: 5
    })
    {:ok, peer} = Effusion.Repo.insert(%Effusion.BTP.Peer{
      torrent: torrent,
      address: %Postgrex.INET{address: {192, 168, 1, 1}},
      port: 8080,
      failcount: 1,
      peer_choking: false,
      peer_interested: true,
      am_choking: false,
      am_interested: true
    })

    {:ok, _request} = Effusion.Repo.insert(%Effusion.BTP.Request{
      block: block,
      peer: peer
    })
  end
end
