defmodule Effusion.IOTest do
  use ExUnit.Case
  alias Effusion.BTP.Torrent
  alias Effusion.BTP.Block
  alias Effusion.BTP.Metainfo
  doctest Effusion.IO


  setup do
    :ets.insert(MetadataTable, {TestHelper.tiny_meta().info_hash, TestHelper.tiny_meta()})
    :ok
  end

  test "writes the contents of the torrent out to a file" do
    Temp.track!

    torrent = TestHelper.tiny_meta().info_hash
    |> Torrent.new()
    |> Torrent.add_block(Block.new(0, 0, "tin"))
    |> Torrent.add_block(Block.new(1, 0, "y\n"))

    {:ok, tmp_path} = Temp.path
    Effusion.IO.write_to(torrent, tmp_path)

    file = File.read! Path.join(tmp_path, "tiny.txt")

    assert file == "tiny\n"
  end

  test "writes the contents of a multi-file torrent out into a directory" do
    Temp.track!
    {:ok, metabin} = File.read "test/hello_world.torrent"
    {:ok, meta} = Metainfo.decode(metabin)

    torrent = meta.info_hash
    |> Torrent.new()
    |> Torrent.add_block(Block.new(0, 0, "Hello\nworld!\n"))

    {:ok, tmp_path} = Temp.path
    Effusion.IO.write_to(torrent, tmp_path)

    hello = File.read! Path.join(tmp_path, "hello_world/hello.txt")
    world = File.read! Path.join(tmp_path, "hello_world/world.txt")

    assert hello == "Hello\n"
    assert world == "world!\n"
  end
end
