defmodule Effusion.DHT.KRPC.Response do
  def encode({:ping, transaction_id, node_id}) do
    response(transaction_id, node_id, "ping")
  end

  def encode({:find_node, transaction_id, node_id, nodes}) do
    compact_nodes = nodes |> Enum.map(&compact_node/1) |> Enum.reduce(fn b, acc -> acc <> b end)
    response(transaction_id, node_id, "find_node", %{"nodes" => compact_nodes})
  end

  def encode({:get_peers_matching, transaction_id, node_id, token, peers}) do
    values = peers |> Enum.map(&compact_peer/1)
    response(transaction_id, node_id, "get_peers", %{
      "token" => token,
      "values" => values
    })
  end

  def encode({:get_peers_nearest, transaction_id, node_id, token, nodes}) do
    compact_nodes = nodes |> Enum.map(&compact_node/1) |> Enum.reduce(fn b, acc -> acc <> b end)
    response(transaction_id, node_id, "get_peers", %{
      "token" => token,
      "nodes" => compact_nodes
    })
  end

  def encode({:announce_peer, transaction_id, node_id}) do
    response(transaction_id, node_id, "announce_peer")
  end

  defp compact_node({node_id, address}) do
    node_id <> compact_peer(address)
  end

  defp compact_peer({{a, b, c, d}, port}) do
    <<a, b, c, d, port::16>>
  end

  defp response(transaction_id, responder_id, reponse_name, args \\ %{}) when is_map(args) do
    %{
      "t" => transaction_id,
      "y" => "r",
      "q" => reponse_name,
      "r" => Map.merge(%{"id" => responder_id}, args)
    }
  end

  def decode(%{
      "t" => transaction_id,
      "q" => "ping",
      "r" => %{"id" => node_id}
    }) do
    {:ping, transaction_id, node_id}
  end

  def decode(%{
    "t" => transaction_id,
    "q" => "find_node",
    "r" => %{
      "id" => node_id,
      "nodes" => nodes
    }
  }) do
    {:find_node, transaction_id, node_id, decode_nodes(nodes)}
  end

  def decode(%{
    "t" => transaction_id,
    "q" => "get_peers",
    "r" => %{
      "id" => node_id,
      "token" => token,
      "values" => addresses
    }
  }) do
    {
      :get_peers_matching,
      transaction_id,
      node_id,
      token,
      Enum.map(addresses, &decode_address/1)
    }
  end

  def decode(%{
    "t" => transaction_id,
    "q" => "get_peers",
    "r" => %{
      "id" => node_id,
      "token" => token,
      "nodes" => nodes
    }
  }) do
    {
      :get_peers_nearest,
      transaction_id,
      node_id,
      token,
      decode_nodes(nodes)
    }
  end

  def decode(%{
      "t" => transaction_id,
      "q" => "announce_peer",
      "r" => %{"id" => node_id}
    }) do
    {:announce_peer, transaction_id, node_id}
  end

  defp decode_nodes(nodes) do
    nodes
    |> :binary.bin_to_list()
    |> Enum.chunk_every(26)
    |> Enum.map(&:erlang.list_to_binary/1)
    |> Enum.map(fn <<node_id::160, ip0, ip1, ip2, ip3, port::16>> ->
      {<<node_id::160>>, {{ip0, ip1, ip2, ip3}, port}}
    end)
  end

  def decode_address(<<ip0, ip1, ip2, ip3, port::16>>) do
    {{ip0, ip1, ip2, ip3}, port}
  end
end
