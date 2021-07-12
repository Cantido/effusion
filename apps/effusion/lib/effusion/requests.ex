defmodule Effusion.Requests do
  @moduledoc """
  Tracks blocks requested from peers.
  """

  defstruct [
    requests: %{}
  ]

  def get_peers_for_block(struct, index, offset, size) do
    Map.get(struct.requests, index, %{})
    |> Map.get({offset, size}, [])
  end

  def get_blocks_for_peer(struct, address) do
    Enum.flat_map(struct.requests, fn {index, block_requests} ->
      Enum.filter(block_requests, fn {_block, peers} ->
        address in peers
      end)
      |> Enum.map(fn {{offset, size}, _peers} ->
        {index, offset, size}
      end)
    end)
  end

  def requested?(requests, address, index, offset, size) do
    Map.has_key?(requests.requests, index) and
    Map.has_key?(requests.requests[index], {offset, size}) and
    (address in requests.requests[index][{offset, size}])
  end

  def add_request(requests, address, index, offset, size) do
    updated_requests =
      Map.update(requests.requests, index, %{{offset, size} => [address]}, fn piece_requests ->
        Map.update(piece_requests, {offset, size}, [address], fn request_addresses ->
          [address | request_addresses]
        end)
      end)

    %__MODULE__{requests | requests: updated_requests}
  end

  def cancel_requests(requests, index, offset, size) do
    if Map.has_key?(requests.requests, index) do
      updated_piece_map = Map.delete(requests.requests[index], {offset, size})
      updated_requests = Map.put(requests.requests, index, updated_piece_map)

      %__MODULE__{requests | requests: updated_requests}
    else
      requests
    end
  end

  def drop_requests_to_peer(requests, address) do
    updated_requests =
      Enum.map(requests.requests, fn {index, requests} ->
        requests =
          Enum.map(requests, fn {block, addresses} ->
            addresses = Enum.reject(addresses, &(&1 == address))
            {block, addresses}
          end)
          |> Map.new()
        {index, requests}
      end)
      |> Map.new()


    %__MODULE__{requests | requests: updated_requests}
  end
end
