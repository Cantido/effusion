defmodule Effusion.Download do
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

  def get_piece(download, piece_index) do
    Map.get(download.pieces, piece_index)
  end

  def block_requests(download, address) do
    Availability.peer_pieces(download.availability, address)
    |> Enum.reject(fn index ->
      Map.get(download.pieces, index) == :written
    end)
    |> Enum.flat_map(fn index ->
      Map.get(download.pieces, index, build_piece(download, index))
      |> Piece.needed_blocks(Application.fetch_env!(:effusion, :block_size))
      |> Enum.map(fn {offset, size} ->
        {index, offset, size}
      end)
    end)
    |> Enum.shuffle()
    |> Enum.take(Application.fetch_env!(:effusion, :max_requests_per_peer))
  end

  def requests_for_block(download, index, offset, size) do
    Map.get(download.requests, index, %{})
    |> Map.get({offset, size}, [])
  end

  def piece_written?(download, piece_index) do
    get_piece(download, piece_index) == :written
  end

  def bytes_left(download) do
    bytes_completed =
      Enum.map(download.pieces, fn {index, piece} ->
        if piece == :written do
          Metadata.piece_size(download.meta.info, index)
        else
          0
        end
      end)
      |> Enum.sum()

    download.meta.info.length - bytes_completed
  end

  def piece_written(download, piece_index) do
    pieces = Map.put(download.pieces, piece_index, :written)

    %__MODULE__{download | pieces: pieces}
  end

  def block_requested(download, address, index, offset, size) do
    requests =
      Map.update(download.requests, index, %{{offset, size} => [address]}, fn piece_requests ->
        Map.update(piece_requests, {offset, size}, [address], fn request_addresses ->
          [address, request_addresses]
        end)
      end)
    %__MODULE__{download | requests: requests}
  end

  def add_data(download, piece_index, offset, data) do
    new_piece =
      build_piece(download, piece_index)
      |> Piece.add_data(offset, data)

    pieces = Map.update(download.pieces, piece_index, new_piece, &Piece.add_data(&1, offset, data))

    %__MODULE__{download | pieces: pieces}
  end

  def add_peer(download, address) do
    %{download | peers: MapSet.put(download.peers, address)}
  end

  def peer_has_piece(download, address, piece_index) when is_integer(piece_index) do
    avail = Availability.peer_has_piece(download.availability, address, piece_index)
    %__MODULE__{download | availability: avail}
  end

  defp build_piece(download, piece_index) do
    %Piece{
      index: piece_index,
      expected_hash: Enum.at(download.meta.info.pieces, piece_index),
      expected_size: Effusion.Metadata.piece_size(download.meta.info, piece_index)
    }
  end
end
