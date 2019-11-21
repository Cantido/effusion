defmodule Effusion.BTP.TorrentRegistry do
  def all_torrents() do
    Registry.select(SessionRegistry, [{{:"$1", :_, :_}, [], [:"$1"]}])
  end
end
