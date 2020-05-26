defmodule Effusion.BTP.TorrentRegistry do

  @moduledoc """
  A registry of all active torrents.
  """

  def all_torrents do
    Registry.select(BTPHandlerRegistry, [{{:"$1", :_, :_}, [], [:"$1"]}])
  end

  def present?(info_hash) do
    case Registry.lookup(BTPHandlerRegistry, info_hash) do
      [_] -> true
      [] -> false
    end
  end
end
