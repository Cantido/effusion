defmodule Effusion.DHT.Nodes do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    AddDHTNode
  }

  def add(primary_node_id, remote_node_id, host, port) do
    CQRS.dispatch(
      %AddDHTNode{
        primary_node_id: Effusion.Hash.encode(primary_node_id),
        node_id: Effusion.Hash.encode(remote_node_id),
        host: to_string(:inet.ntoa(host)),
        port: port
      }
    )
  end
end
