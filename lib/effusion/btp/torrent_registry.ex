defmodule Effusion.BTP.TorrentRegistry do
  def all_torrents() do
    Registry.select(SessionRegistry, [{{:"$1", :_, :_}, [], [:"$1"]}])
  end

  def present?(info_hash) do
    case Registry.lookup(SessionRegistry, info_hash) do
      [_] -> true
      [] -> false
    end
  end
end
