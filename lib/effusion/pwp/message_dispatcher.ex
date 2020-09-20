defmodule Effusion.PWP.MessageDispatcher do
  def dispatch(msg, info_hash, peer_id) do
    Queutils.BlockingProducer.push(MessageProducer, {info_hash, peer_id, msg})
  end
end
