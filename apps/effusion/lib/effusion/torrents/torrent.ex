defmodule Effusion.Torrent do
  alias Effusion.Availability
  alias Effusion.Requests
  alias Effusion.Metadata

  @enforce_keys [
    :meta
  ]
  defstruct [
    id: nil,
    meta: nil,
    state: :paused,
    block_size: 16384,
    bytes_uploaded: 0,
    bytes_downloaded: 0,
    verified_pieces: IntSet.new(),
    written_blocks: IntSet.new(),
    availability: %Availability{},
    requests: %Requests{}
  ]

  def new(meta) do
    %__MODULE__{
      id: Base.encode16(meta.info_hash, case: :lower),
      meta: meta
    }
  end

  def running?(torrent) do
    torrent.state == :running
  end

  def continue(torrent) do
    %{torrent | state: :running}
  end

  def paused?(torrent) do
    torrent.state == :paused
  end

  def pause(torrent) do
    %{torrent | state: :paused}
  end

  def get_bitfield(torrent) do
    bitfield = IntSet.bitstring(torrent.verified_pieces, byte_align: true)

    piece_count = Enum.count(torrent.meta.info.pieces)
    bitfield_length_bytes = ceil(piece_count / 8)
    end_pad_bytes_count = bitfield_length_bytes - byte_size(bitfield)
    end_pad_bits_count = end_pad_bytes_count * 8

    if end_pad_bytes_count > 0 do
      bitfield <> <<0::integer-size(end_pad_bits_count)>>
    else
      bitfield
    end
  end

  def block_requests(torrent, address) do
    available_pieces = Availability.peer_pieces(torrent.availability, address)
    pieces_we_have = torrent.verified_pieces

    needed_available_pieces = IntSet.difference(available_pieces, pieces_we_have)

    blocks_we_need = IntSet.inverse(torrent.written_blocks, block_count(torrent))

    needed_available_blocks =
      Enum.map(blocks_we_need, fn block_index ->
        {block_index, block_size(torrent, block_index)}
      end)
      |> Enum.map(fn {block_index, block_size} ->
        {i, o} = block_piece_and_offset(torrent, block_index)
        {i, o, block_size}
      end)
      |> Enum.filter(fn {piece_index, _offset, _size} ->
        Enum.member?(needed_available_pieces, piece_index)
      end)
      |> Enum.reject(fn {index, offset, size} ->
        Requests.requested?(torrent.requests, address, index, offset, size)
      end)

    previous_request_count = Enum.count(Requests.get_blocks_for_peer(torrent.requests, address))

    additional_request_count = Application.fetch_env!(:effusion, :max_requests_per_peer) - previous_request_count

    needed_available_blocks
    |> Enum.shuffle()
    |> Enum.take(additional_request_count)
  end

  def requests_for_block(torrent, index, offset, size) do
    Requests.get_peers_for_block(torrent.requests, index, offset, size)
  end

  def piece_verified?(torrent, piece_index) do
    Enum.member?(torrent.verified_pieces, piece_index)
  end

  def piece_written?(torrent, piece_index) do
    block_indices = piece_block_range(torrent, piece_index)

    Enum.all?(block_indices, fn block_index ->
      Enum.member?(torrent.written_blocks, block_index)
    end)
  end

  def block_written?(torrent, piece_index, offset) do
    block_index = block_index(torrent, piece_index, offset)

    Enum.member?(torrent.written_blocks, block_index)
  end

  def progress(torrent) do
    completed = bytes_completed(torrent)

    {completed, torrent.meta.info.length}
  end

  def bytes_completed(torrent) do
    Enum.map(torrent.verified_pieces, fn piece_index ->
      Metadata.piece_size(torrent.meta.info, piece_index)
    end)
    |> Enum.sum()
  end

  def bytes_left(torrent) do
    bytes_completed = bytes_completed(torrent)

    torrent.meta.info.length - bytes_completed
  end

  def block_written(torrent, piece_index, offset) do
    block_index = block_index(torrent, piece_index, offset)

    blocks = IntSet.put(torrent.written_blocks, block_index)

    %__MODULE__{torrent | written_blocks: blocks}
  end

  def piece_verified(torrent, piece_index) do
    pieces = IntSet.put(torrent.verified_pieces, piece_index)
    %{torrent | verified_pieces: pieces}
  end

  def piece_failed_verification(torrent, piece_index) do
    written_blocks =
      piece_block_range(torrent, piece_index)
      |> Enum.reduce(torrent.written_blocks, fn block_index, written_blocks ->
        IntSet.delete(written_blocks, block_index)
      end)
    %{torrent | written_blocks: written_blocks}
  end

  def block_requested(torrent, address, index, offset, size) do
    requests = Requests.add_request(torrent.requests, address, index, offset, size)

    %__MODULE__{torrent | requests: requests}
  end

  def drop_requests(torrent, address) do
    requests = Requests.drop_requests_to_peer(torrent.requests, address)

    %__MODULE__{torrent | requests: requests}
  end

  def drop_requests(torrent, index, offset, size) do
    requests = Requests.cancel_requests(torrent.requests, index, offset, size)

    %__MODULE__{torrent | requests: requests}
  end

  def peer_has_piece(torrent, address, piece_index) when is_integer(piece_index) do
    avail = Availability.peer_has_piece(torrent.availability, address, piece_index)
    %__MODULE__{torrent | availability: avail}
  end

  defp block_count(torrent) do
    nominal_block_size = min(torrent.block_size, torrent.meta.info.piece_length)

    ceil(torrent.meta.info.length / nominal_block_size)
  end

  defp block_index(torrent, piece_index, offset) do
    nominal_block_size = min(torrent.meta.info.piece_length, torrent.block_size)
    blocks_per_piece = div(torrent.meta.info.piece_length, nominal_block_size)
    blocks_per_piece * piece_index + div(offset, nominal_block_size)
  end

  defp block_piece_and_offset(torrent, block_index) do
    blocks_per_piece =
      if torrent.meta.info.piece_length >= torrent.block_size do
        div(torrent.meta.info.piece_length, torrent.block_size)
      else
        1
      end
    piece_index = div(block_index, blocks_per_piece)
    block_index_within_piece = rem(block_index, blocks_per_piece)
    block_offset = block_index_within_piece * torrent.block_size

    {piece_index, block_offset}
  end

  defp block_size(torrent, block_index) do
    if block_index == (block_count(torrent) - 1) do
      {piece_index, _block_offset} = block_piece_and_offset(torrent, block_index)
      piece_size = Effusion.Metadata.piece_size(torrent.meta.info, piece_index)

      rem(piece_size, torrent.block_size)
    else
      min(torrent.block_size, torrent.meta.info.piece_length)
    end
  end

  defp piece_block_range(torrent, piece_index) do
    nominal_piece_size = torrent.meta.info.piece_length
    nominal_block_size = min(torrent.block_size, nominal_piece_size)
    nominal_blocks_per_piece = ceil(nominal_piece_size / nominal_block_size)

    actual_piece_size = Effusion.Metadata.piece_size(torrent.meta.info, piece_index)
    blocks_in_piece = ceil(actual_piece_size / nominal_block_size)

    first_block_index = nominal_blocks_per_piece * piece_index

    Effusion.Range.from_poslen(first_block_index, blocks_in_piece)
  end
end
