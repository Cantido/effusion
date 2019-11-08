defmodule Effusion.BTP.PiecePicker do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Pieces
  import Effusion.Math
  require Logger

  @moduledoc """
  Strategies for selecting which pieces of a torrent to download.
  """

  @doc """
  Get the next block to request, or `nil` if nothing can be requested.
  """
  def next_block(torrent, peers, block_size) do
    if Pieces.all_present?(torrent) do
      nil
    else
      poss = blocks_available(torrent, peers, block_size)

      if Enum.any?(poss) do
        Enum.random(poss)
      else
        nil
      end
    end
  end

  # General rule: 10 requests per 5 Mbps download speed
  @max_requests_per_peer 200

  def next_blocks(torrent, peers, block_size) do
    if Pieces.all_present?(torrent) do
      []
    else
      blocks_available(torrent, peers, block_size)
      |> Enum.chunk_by(&elem(&1, 0))
      |> Enum.flat_map(fn blocks_for_peer ->
        peer_id = Enum.at(blocks_for_peer, 0) |> elem(0)
        peer = Enum.find(peers, &(&1.remote_peer_id == peer_id))
        count_already_requested = peer.blocks_we_requested |> Enum.count()
        count_to_request = max(@max_requests_per_peer - count_already_requested, 0)

        Enum.take_random(blocks_for_peer, count_to_request)
      end)
    end
  end

  @doc """
  Returns a set of ID-block pairs, of all blocks that can be requested, and from what peers
  """
  def blocks_available(torrent, peers, block_size) do
    we_have = Pieces.bitfield(torrent)
    info = torrent.info

    peers
    |> available_pieces_by_peer()
    |> select_required_pieces(we_have)
    |> expand_piece_sets()
    |> pieces_to_blocks(info, block_size)
    |> reject_blocks_present_in_torrent(torrent)
    |> reject_blocks_already_requested(peers)
  end

  defp available_pieces_by_peer(peers) do
    peers
    |> Enum.map(fn p -> {p.remote_peer_id, p.has} end)
  end

  # Selects peers that have pieces in the we_have set
  defp select_required_pieces(pieces, we_have) do
    pieces
    |> Enum.map(fn {id, has} -> {id, IntSet.difference(has, we_have)} end)
    |> Enum.reject(fn {_id, has} -> Enum.empty?(has) end)
  end

  # Expands `{peer_id, pieces}` into many `{peer_id, piece}` enums
  defp expand_piece_sets(peers) do
    peers
    |> Enum.flat_map(fn {id, has} -> for p <- has, do: {id, p} end)
  end

  # Splits `{peer_id, piece}` into many `{peer_id, block}` enums
  defp pieces_to_blocks(pieces, info, block_size) do
    pieces
    |> Enum.map(fn {id, p} -> {id, Block.id(p, 0, piece_size(p, info))} end)
    |> Enum.flat_map(fn {id, p} -> for b <- Block.split(p, block_size), do: {id, Block.id(b)} end)
  end

  # Filters out blocks that already exist in `torrent`
  defp reject_blocks_present_in_torrent(blocks, torrent) do
    Logger.debug("Blocks before filtering out pieces we have: #{inspect(blocks)}")

    blocks =
      blocks
      |> Enum.reject(fn {_id, b} -> Pieces.has_block?(torrent, b) end)

    Logger.debug("Blocks after filtering out pieces we have: #{inspect(blocks)}")
    blocks
  end

  defp reject_blocks_already_requested(blocks, peers) do
    Logger.debug("peers with requests: #{inspect(peers)}")

    requested_blocks =
      peers
      |> Enum.flat_map(fn p ->
        Enum.map(p.blocks_we_requested, fn b ->
          {p.remote_peer_id, Block.id(b)}
        end)
      end)
      |> MapSet.new()

    Logger.debug(
      "Blocks we've already requested (from the peers above): #{inspect(requested_blocks)}"
    )

    Logger.debug("possible blocks to request: #{inspect(blocks)}")

    new_blocks =
      blocks
      |> MapSet.new()
      |> MapSet.difference(requested_blocks)

    Logger.debug("new requests to make: #{inspect(new_blocks)}")

    new_blocks
  end

  defp piece_size(index, info) do
    {whole_piece_count, last_piece_size} = divrem(info.length, info.piece_length)
    last_piece_index = whole_piece_count

    if index == last_piece_index do
      last_piece_size
    else
      info.piece_length
    end
  end
end
