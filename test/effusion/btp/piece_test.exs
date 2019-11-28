defmodule Effusion.BTP.PieceTest do
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

    Effusion.Repo.insert(%Effusion.BTP.Piece{
      torrent: torrent,
      index: 0,
      hash: "12345678901234567890",
      size: 1_000_000
    })
  end
end
