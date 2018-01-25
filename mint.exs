{:ok, file} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"

Effusion.add_torrent(file, "Effusion Experiment!", {127, 0, 0, 1}, 4040)
