{:ok, file} = File.read "test/lovecraft.torrent"

{:ok, meta} = Effusion.BTP.Metainfo.decode(file)

Effusion.download(meta, File.cwd!)
