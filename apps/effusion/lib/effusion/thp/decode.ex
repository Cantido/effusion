defmodule Effusion.THP.Decode do
  @moduledoc """
  Decodes a tracker response.
  """

  @peer_names %{
    "ip" => :ip,
    "port" => :port,
    "peer id" => :peer_id
  }

  @doc """
  Decodes a tracker peer response.
  """
  def decode_peers(peers)

  def decode_peers(peers) when is_binary(peers) do
    decode_compact_peers(peers)
  end

  def decode_peers(peers) when is_list(peers) do
    decode_standard_peers(peers)
  end

  @doc """
  Decodes a binary containing compact peers.

  ## Examples

      iex> Effusion.THP.Decode.decode_compact_peers(<<192, 168, 1, 1, 255, 255>>)
      [%{port: 65_535, ip: {192, 168, 1, 1}}]
  """
  def decode_compact_peers(bin)

  def decode_compact_peers(<<>>), do: []

  def decode_compact_peers(peers) when is_binary(peers) do
    peers
    |> :binary.bin_to_list()
    |> Enum.chunk_every(6)
    |> Enum.map(&decode_compact_peer/1)
  end

  defp decode_compact_peer([ip0, ip1, ip2, ip3, p0, p1])
       when ip0 in 0..255 and
              ip1 in 0..255 and
              ip2 in 0..255 and
              ip3 in 0..255 and
              p0 in 0..255 and
              p1 in 0..255 do
    <<port::16>> = <<p0, p1>>

    %{
      ip: {ip0, ip1, ip2, ip3},
      port: port
    }
  end

  @doc """
  Decodes a list of peers in the non-compact representation.

  ## Examples

      iex> Effusion.THP.Decode.decode_standard_peers([%{
      ...>   "ip" => "192.168.1.1",
      ...>   "port" => 65_535,
      ...>   "peer id" => "12345678901234567890"
      ...> }])
      [%{ip: {192, 168, 1, 1}, port: 65_535, peer_id: "12345678901234567890"}]

      iex> Effusion.THP.Decode.decode_standard_peers([%{
      ...>   "ip" => "fd4a:6f02:ec34:1201:0:0:0:0",
      ...>   "port" => 65_535,
      ...>   "peer id" => "12345678901234567890"
      ...> }])
      [%{ip: {64842, 28418, 60468, 4609, 0, 0, 0, 0}, port: 65_535, peer_id: "12345678901234567890"}]

      iex> Effusion.THP.Decode.decode_standard_peers([%{
      ...>   "ip" => "peer.example.com",
      ...>   "port" => 65_535,
      ...>   "peer id" => "12345678901234567890"
      ...> }])
      [%{ip: "peer.example.com", port: 65_535, peer_id: "12345678901234567890"}]
  """
  def decode_standard_peers(list)

  def decode_standard_peers([]), do: []

  def decode_standard_peers(peers) when is_list(peers) do
    Enum.map(peers, &decode_standard_peer/1)
  end

  defp decode_standard_peer(peer) do
    peer
    |> Effusion.Map.rename_keys(@peer_names)
    |> Map.update!(:ip, &parse_address/1)
  end

  defp parse_address(addr) do
    with addr_charlist <- to_charlist(addr),
         {:ok, ip} <- :inet.parse_address(addr_charlist) do
      ip
    else
      # allow for hostnames
      {:error, :einval} when byte_size(addr) in 1..253 -> addr
    end
  end
end
