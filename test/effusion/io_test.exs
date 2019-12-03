defmodule Effusion.IOTest do
  use ExUnit.Case, async: true
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Metainfo
  alias Effusion.BTP.Torrent
  doctest Effusion.IO

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Effusion.Repo)
  end

  setup do
    single_file_meta = TestHelper.tiny_meta()
    Torrent.insert(single_file_meta)

    Pieces.add_block(single_file_meta.info_hash, %{index: 0, offset: 0, data: "tin"})
    Pieces.add_block(single_file_meta.info_hash, %{index: 1, offset: 0, data: "y\n"})

    %{single_file_meta: single_file_meta}
  end

  setup do
    {:ok, metabin} = File.read("test/hello_world.torrent")
    {:ok, multi_file_meta} = Metainfo.decode(metabin)
    Torrent.insert(multi_file_meta)

    Pieces.add_block(multi_file_meta.info_hash, %{index: 0, offset: 0, data: "Hello\nworld!\n"})

    %{multi_file_meta: multi_file_meta}
  end

  setup do
    Temp.track!()

    {:ok, file} = Temp.path()
    old_destination = Application.get_env(:effusion, :download_destination)
    Application.put_env(:effusion, :download_destination, file)

    on_exit(fn ->
      Application.put_env(:effusion, :download_destination, old_destination)
      File.rm_rf(file)
    end)

    %{destfile: file}
  end

  test "writes the contents of the torrent out to a file", %{destfile: file, single_file_meta: meta} do
    Effusion.IO.write_piece(meta.info_hash, %{index: 0})
    Effusion.IO.write_piece(meta.info_hash, %{index: 1})

    file = File.read!(Path.join(file, "tiny.txt"))

    assert file == "tiny\n"
  end

  test "writes the contents of a multi-file torrent out into a directory", %{destfile: file, multi_file_meta: meta} do
    Effusion.IO.write_piece(meta.info_hash, %{index: 0})

    hello = File.read!(Path.join(file, "hello_world/hello.txt"))
    world = File.read!(Path.join(file, "hello_world/world.txt"))

    assert hello == "Hello\n"
    assert world == "world!\n"
  end
end
