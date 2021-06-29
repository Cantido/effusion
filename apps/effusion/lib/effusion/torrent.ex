defmodule Effusion.Torrent do
  alias Effusion.Availability
  alias Effusion.Piece
  alias Effusion.Metadata

  require Logger

  @enforce_keys [
    :meta
  ]
  defstruct [
    meta: nil,
    bytes_uploaded: 0,
    bytes_downloaded: 0,
    pieces: %{},
    peers: MapSet.new(),
    availability: %Availability{},
    requests: %{}
  ]

  def get_piece(torrent, piece_index) do
    Map.get(torrent.pieces, piece_index)
  end

  def block_requests(torrent, address) do
    Availability.peer_pieces(torrent.availability, address)
    |> Enum.reject(fn index ->
      Map.get(torrent.pieces, index) == :written
    end)
    |> Enum.flat_map(fn index ->
      Map.get(torrent.pieces, index, build_piece(torrent, index))
      |> Piece.needed_blocks(Application.fetch_env!(:effusion, :block_size))
      |> Enum.map(fn {offset, size} ->
        {index, offset, size}
      end)
    end)
    |> Enum.shuffle()
    |> Enum.take(Application.fetch_env!(:effusion, :max_requests_per_peer))
  end

  def requests_for_block(torrent, index, offset, size) do
    Map.get(torrent.requests, index, %{})
    |> Map.get({offset, size}, [])
  end

  def piece_written?(torrent, piece_index) do
    get_piece(torrent, piece_index) == :written
  end

  def bytes_left(torrent) do
    bytes_completed =
      Enum.map(torrent.pieces, fn {index, piece} ->
        if piece == :written do
          Metadata.piece_size(torrent.meta.info, index)
        else
          0
        end
      end)
      |> Enum.sum()

      torrent.meta.info.length - bytes_completed
  end

  def piece_written(torrent, piece_index) do
    pieces = Map.put(torrent.pieces, piece_index, :written)

    %__MODULE__{torrent | pieces: pieces}
  end

  def block_requested(torrent, address, index, offset, size) do
    requests =
      Map.update(torrent.requests, index, %{{offset, size} => [address]}, fn piece_requests ->
        Map.update(piece_requests, {offset, size}, [address], fn request_addresses ->
          [address, request_addresses]
        end)
      end)
    %__MODULE__{torrent | requests: requests}
  end

  def add_data(torrent, piece_index, offset, data) do
    new_piece =
      build_piece(torrent, piece_index)
      |> Piece.add_data(offset, data)

    pieces = Map.update(torrent.pieces, piece_index, new_piece, &Piece.add_data(&1, offset, data))

    %__MODULE__{torrent | pieces: pieces}
  end

  def add_peer(torrent, address) do
    %{torrent | peers: MapSet.put(torrent.peers, address)}
  end

  def peer_has_piece(torrent, address, piece_index) when is_integer(piece_index) do
    avail = Availability.peer_has_piece(torrent.availability, address, piece_index)
    %__MODULE__{torrent | availability: avail}
  end

  defp build_piece(torrent, piece_index) do
    %Piece{
      index: piece_index,
      expected_hash: Enum.at(torrent.meta.info.pieces, piece_index),
      expected_size: Effusion.Metadata.piece_size(torrent.meta.info, piece_index)
    }
  end
end
