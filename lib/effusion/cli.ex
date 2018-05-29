defmodule Effusion.CLI do

  @aliases [o: :output]
  @strict [output: :string]

  def main(argv) do
    {[output: dest], [file], invalid} = OptionParser.parse(argv, strict: @strict, aliases: @aliases)
    Enum.each(invalid, fn(i) ->
      IO.warn("Invalid option #{i}")
    end)

    {:ok, _torrent} = Effusion.download(file, dest)
  end
end
