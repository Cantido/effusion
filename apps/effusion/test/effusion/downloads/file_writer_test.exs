defmodule Effusion.Downloads.EventHandlers.FileWriterTest do
  use ExUnit.Case
  alias Effusion.Downloads.EventHandlers.FileWriter
  alias Effusion.Downloads.Events.PieceHashSucceeded

  doctest Effusion.Downloads.EventHandlers.FileWriter

  setup do
    meta =
      TestHelper.tiny_meta()
      |> Map.update!(:info_hash, &Effusion.Hash.encode/1)

    %{single_file_meta: meta}
  end

  setup do
    {:ok, metabin} = File.read("test/hello_world.torrent")
    {:ok, multi_file_meta} = Metatorrent.decode(metabin)
    meta = Map.update!(multi_file_meta, :info_hash, &Effusion.Hash.encode/1)

    %{multi_file_meta: meta}
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

  test "writes the contents of the torrent out to a file", %{
    destfile: file,
    single_file_meta: meta
  } do
    :ok =
      FileWriter.handle(
        %PieceHashSucceeded{
          info_hash: meta.info_hash,
          info: meta.info,
          index: 0,
          data: Base.encode64("tin")
        },
        %{}
      )

    :ok =
      FileWriter.handle(
        %PieceHashSucceeded{
          info_hash: meta.info_hash,
          info: meta.info,
          index: 1,
          data: Base.encode64("y\n")
        },
        %{}
      )

    file = File.read!(Path.join(file, "tiny.txt"))

    assert file == "tiny\n"
  end

  test "writes the contents of a multi-file torrent out into a directory", %{
    destfile: file,
    multi_file_meta: meta
  } do
    :ok =
      FileWriter.handle(
        %PieceHashSucceeded{
          info_hash: meta.info_hash,
          info: meta.info,
          index: 0,
          data: Base.encode64("Hello\nworld!\n")
        },
        %{}
      )

    hello = File.read!(Path.join(file, "hello_world/hello.txt"))
    world = File.read!(Path.join(file, "hello_world/world.txt"))

    assert hello == "Hello\n"
    assert world == "world!\n"
  end
end
