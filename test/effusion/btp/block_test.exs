defmodule Effusion.BTP.BlockTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Block
  alias Effusion.Repo
  doctest Effusion.BTP.Block

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Effusion.Repo, { :shared, self() })
    :ok
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
      size: 5
    })

    {:ok, block} = Effusion.Repo.insert(%Effusion.BTP.Block{
      piece: piece,
      offset: 0,
      data: "tiny\n",
      size: 5
    })

    Repo.get(Block, block.id)
  end

end
