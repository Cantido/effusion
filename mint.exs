{:ok, file} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"

{:ok, meta} = Effusion.BTP.Metainfo.decode(file)

Effusion.download(meta, File.cwd!)
