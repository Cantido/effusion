defmodule Effusion.BTP.RequestTest do
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
  end


  test "insert" do
    {:ok, torrent} = Effusion.Repo.insert(%Effusion.BTP.Torrent{
      info_hash: "12345678901234567890",
      name: "linuxmint-19.2-cinnamon-64bit.iso",
      started: Timex.now() |> DateTime.truncate(:second),
      announce: "http://example.com/announce"
    })

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
