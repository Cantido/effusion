defmodule Effusion.DHT.KRPC.Query do
  import Effusion.DHT

  def encode({:ping, transaction_id, sender_id}) when is_node_id(sender_id) do
    query(transaction_id, sender_id, "ping")
  end

  def encode({:find_node, transaction_id, sender_id, target_id}) when is_node_id(sender_id) and is_node_id(target_id) do
    query(transaction_id, sender_id, "find_node", %{"target" => target_id})
  end

  def encode({:get_peers, transaction_id, sender_id, info_hash}) when is_node_id(sender_id) and is_info_hash(info_hash) do
    query(transaction_id, sender_id, "get_peers", %{"info_hash" => info_hash})
  end

  def encode({:announce_peer, transaction_id, sender_id, info_hash, port, token})
    when is_node_id(sender_id)
     and is_info_hash(info_hash)
     and is_inet_port(port)
     and is_binary(token)do

    query(transaction_id, sender_id, "announce_peer", %{
      "info_hash" => info_hash,
      "port" => port,
      "token" => token,
      "implied_port" => 0
      })
  end

  def encode({:announce_peer, transaction_id, sender_id, info_hash, port, token, :implied_port})
    when is_node_id(sender_id)
     and is_info_hash(info_hash)
     and is_inet_port(port)
     and is_binary(token) do

    query(transaction_id, sender_id, "announce_peer", %{
      "info_hash" => info_hash,
      "port" => port,
      "token" => token,
      "implied_port" => 1
      })
  end

  defp query(transaction_id, sender_id, query_name, args \\ %{}) when is_map(args) do
    %{
      "t" => transaction_id,
      "y" => "q",
      "q" => query_name,
      "a" => Map.merge(%{"id" => sender_id}, args)
    }
  end

  def decode(%{"q" => "ping", "t" => transaction_id, "a" => %{"id" => sender_id}}) do
    {:ping, transaction_id, sender_id}
  end

  def decode(%{"q" => "find_node", "t" => transaction_id,"a" => %{"id" => sender_id, "target" => target_id}}) do
    {:find_node, transaction_id, sender_id, target_id}
  end

  def decode(%{"q" => "get_peers", "t" => transaction_id,"a" => %{"id" => sender_id, "info_hash" => info_hash}}) do
    {:get_peers, transaction_id, sender_id, info_hash}
  end

  def decode(%{"q" => "announce_peer", "t" => transaction_id,
               "a" => %{"id" => sender_id,
                        "info_hash" => info_hash,
                        "port" => port,
                        "token" => token,
                        "implied_port" => 1}}) do
    {:announce_peer, transaction_id, sender_id, info_hash, port, token, :implied_port}
  end

  def decode(%{"q" => "announce_peer", "t" => transaction_id,
               "a" => %{"id" => sender_id,
                        "info_hash" => info_hash,
                        "port" => port,
                        "token" => token}}) do
    {:announce_peer, transaction_id, sender_id, info_hash, port, token}
  end
end
