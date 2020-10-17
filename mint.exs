{:ok, file} = File.read "test/linuxmint-19.2-cinnamon-64bit.iso.torrent"

{:ok, meta} = Metatorrent.decode(file)

Effusion.download(meta)
