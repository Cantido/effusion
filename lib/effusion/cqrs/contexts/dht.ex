defmodule Effusion.CQRS.Contexts.DHT do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    HandlePeersMatching
  }

  def dispatch_query(query) do
    raise "TODO"
  end

  def dispatch_response({:get_peers_matching, transaction_id, node_id, token, peers}) do
    :ok = CQRS.dispatch(
      %HandlePeersMatching{
        transaction_id: transaction_id,
        node_id: Effusion.Hash.encode(node_id),
        token: token,
        peers: Enum.map(peers, fn {host, port} ->
          {to_string(:inet.ntoa(host)), port}
        end)
      }
    )
  end
end
