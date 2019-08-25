defmodule Effusion.BTP.PieceSelection do
  require Logger
  alias Effusion.BTP.Block
  alias Effusion.BTP.Torrent
  import Effusion.Math

  @moduledoc """
  Strategies for selecting which pieces of a torrent to download.
  """

  @doc """
  Get the next block to request, or `nil` if nothing can be requested.
  """
  def next_block(torrent, peers, block_size) do
    if Torrent.all_present?(torrent) do
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

  @doc """
  Returns a set of ID-block pairs, of all blocks that can be requested, and from what peers
  """
  def blocks_available(torrent, peers, block_size) do
    we_have = Torrent.bitfield(torrent)
    info = torrent.info
    peers
    |> select_peers_with_required_pieces(we_have)
    |> expand_piece_sets()
    |> pieces_to_blocks(info, block_size)
    |> reject_blocks_present_in_torrent()
  end

  # select peers that have pieces we need
  defp select_peers_with_required_pieces(peers, we_have) do
    peers
    |> Enum.map(fn p -> {p.remote_peer_id, p.has} end)
    |> Enum.map(fn {id, has} -> {id, IntSet.difference(has, we_have)} end)
    |> Enum.reject(fn {_id, has} -> Enum.empty?(has) end)
  end

  # expand {peer_id, pieces} into many {peer_id, piece} enums
  defp expand_piece_sets(peers) do
    peers
    |> Enum.flat_map(fn {id, has} -> for p <- has, do: {id, p} end)
  end

  # split {peer_id, piece} into many {peer_id, block} enums
  defp pieces_to_blocks(pieces, info, block_size) do
    pieces
    |> Enum.map(fn {id, p} -> {id, Block.id(p, 0, piece_size(p, info))} end)
    |> Enum.flat_map(fn {id, p} -> for b <- Block.split(p, block_size), do: {id, b} end)
  end

  defp reject_blocks_present_in_torrent(blocks, torrent) do
    blocks
    |> Enum.reject(fn {_id, b} -> Torrent.has_block?(torrent, b) end)
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
