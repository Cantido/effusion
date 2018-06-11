defmodule Effusion.CLI do
  alias Effusion.BTP.Metainfo

  @aliases [o: :output]
  @strict [output: :string]

  def main(argv) do
    {[output: dest], [file], invalid} = OptionParser.parse(argv, strict: @strict, aliases: @aliases)
    Enum.each(invalid, fn(i) ->
      IO.warn("Invalid option #{i}")
    end)

    {:ok, metabin} = File.read file
    {:ok, meta} = Metainfo.decode(metabin)

    {:ok, _torrent} = Effusion.download(meta, dest)
  end
end
