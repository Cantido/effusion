defmodule Effusion.PWP.MessageDispatcher do
  require Logger
  def dispatch(msg, info_hash, peer_id) do
    Logger.debug("***** Got message #{inspect msg}")
    :ok = Effusion.CQRS.Contexts.Downloads.handle_message(info_hash, peer_id, msg)
    :ok
  end
end
