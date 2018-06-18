defmodule Effusion.IOTest do
  use ExUnit.Case
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Block
  doctest Effusion.IO

  test "writes the contents of the torrent out to a file" do
    Temp.track!

    torrent = TestHelper.tiny_meta().info
    |> Torrent.new()
    |> Torrent.add_block(Block.new(0, 0, "tin"))
    |> Torrent.add_block(Block.new(1, 0, "y\n"))

    {:ok, tmp_path} = Temp.path
    Effusion.IO.write_to(torrent, tmp_path)

    file = File.read! Path.join(tmp_path, "tiny.txt")

    assert file == "tiny\n"
  end
end
