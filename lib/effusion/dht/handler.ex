defmodule Effusion.DHT.Handler do
  alias Effusion.CQRS.Contexts.DHT, as: DHTContext
  def handle_query(query) do
    raise "TODO"
  end

  def handle_response({:get_peers_matching, transaction_id, node_id, token, peers}) do
    DHTContext.handle_peers_matching(transaction_id, node_id, token, peers)
  end
end
