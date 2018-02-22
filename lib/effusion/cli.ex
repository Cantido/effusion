defmodule Effusion.CLI do
  def main(args) do
    [file, dest] = args
    {:ok, _torrent} = Effusion.download(file, dest)
    IO.puts "done!"
  end
end
