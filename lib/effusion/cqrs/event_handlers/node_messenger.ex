defmodule Effusion.CQRS.EventHandlers.NodeMessenger do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.DHT.KRPC.Query
  alias Effusion.CQRS.Events.{
    GettingPeers
  }

  def handle(
    %GettingPeers{
      primary_node_id: sender_id,
      host: host,
      port: port,
      info_hash: info_hash,
      transaction_id: transaction_id
    },
    _metadata
  ) do
    send_query(
      {
        :get_peers,
        transaction_id,
        Effusion.Hash.decode(sender_id),
        Effusion.Hash.decode(info_hash)
      },
      host,
      port
      )
  end

  defp send_query(query, host, port) do
    encoded_query =
      Query.encode(query)
      |> Bento.encode!()

    # If Port == 0, the underlying OS assigns a free UDP port, use inet:port/1 to retrieve it.
    {:ok, socket} = :gen_udp.open(0)
    :ok = :gen_udp.send(socket, host, port, encoded_query)
    :ok = :gen_udp.close(socket)
  end
end
