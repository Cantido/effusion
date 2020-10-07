defmodule Effusion.CQRS.Aggregates.Torrent do
  alias Effusion.CQRS.Commands.{
    HandleCompletedDownload,
    AddTorrent,
    StartDownload,
    StoreBlock
  }
  alias Effusion.CQRS.Events.{
    BlockStored,
    AllPiecesVerified,
    DownloadStarted,
    DownloadFailed,
    DownloadCompleted,
    PieceHashSucceeded,
    PieceHashFailed,
    TorrentAdded
  }
  alias Commanded.Aggregate.Multi
  require Logger

  defstruct [
    announce: nil,
    announce_list: [],
    comment: nil,
    created_by: nil,
    creation_date: nil,
    info: nil,
    info_hash: nil,
    pieces: %{},
    verified: IntSet.new()
  ]

  def execute(
    %__MODULE__{info_hash: nil},
    %AddTorrent{info_hash: info_hash} = command
  ) do
    %TorrentAdded{
      announce: command.announce,
      announce_list: command.announce_list,
      comment: command.comment,
      created_by: command.created_by,
      info: command.info,
      info_hash: command.info_hash
    }
  end

  def execute(
    %__MODULE__{info_hash: aggregate_info_hash},
    %AddTorrent{info_hash: command_info_hash} = command
  ) when aggregate_info_hash == command_info_hash do
    {:error, :torrent_already_exists}
  end

  def execute(
    %__MODULE__{info_hash: torrent_info_hash, verified: verified, info: info, announce: announce} = torrent,
    %StartDownload{info_hash: info_hash} = command
  ) when not is_nil(torrent_info_hash) do
    bytes_left = info.length - (Enum.count(verified) * info.piece_length)

    %DownloadStarted{
      info_hash: command.info_hash,
      announce: announce,
      bytes_left: bytes_left,
      announce_list: torrent.announce_list,
      comment: torrent.comment,
      created_by: torrent.created_by,
      info: torrent.info,
      info_hash: torrent.info_hash,
    }
  end


  def execute(
    %__MODULE__{} = torrent,
    %StoreBlock{info_hash: info_hash, from: from, index: index, offset: offset, data: block_data}
  ) do
    torrent
    |> Multi.new()
    |> Multi.execute(&store_block(&1, from, index, offset, block_data))
    |> Multi.execute(&check_for_finished_piece(&1, index))
    |> Multi.execute(&check_for_finished_torrent/1)
  end

  defp store_block(%__MODULE__{info_hash: info_hash, pieces: pieces}, from, index, offset, data) do
    pieces =
      Map.update(
        pieces,
        index,
        [%{offset: offset, data: data}],
        & [%{offset: offset, data: data} | &1])

    %BlockStored{
      from: from,
      info_hash: info_hash,
      index: index,
      offset: offset,
      data: data,
      pieces: pieces
    }
  end

  defp check_for_finished_piece(%__MODULE__{info_hash: info_hash, pieces: pieces, info: info}, index) do
    piece_size =
      pieces[index]
      |> Enum.map(& &1.data)
      |> Enum.map(&byte_size/1)
      |> Enum.sum()

    expected_piece_count = Enum.count(info.pieces)
    last_piece_size = info.length - (expected_piece_count - 1) * info.piece_length

    normal_piece_done = index < (expected_piece_count - 1) and piece_size == info.piece_length
    last_piece_done = index == (expected_piece_count - 1) and last_piece_size

    if normal_piece_done or last_piece_done do
      piece_data =
        pieces[index]
        |> Enum.map(& &1.data)
        |> Enum.join()

      if Effusion.Hash.calc(piece_data) == Enum.at(info.pieces, index) do
        %PieceHashSucceeded{info_hash: info_hash, index: index, data: piece_data}
      else
        %PieceHashFailed{info_hash: info_hash, index: index}
      end
    end
  end

  defp check_for_finished_torrent(%__MODULE__{info_hash: info_hash, verified: verified, info: info}) do
    if Enum.count(verified) == Enum.count(info.pieces) do
      %AllPiecesVerified{info_hash: info_hash}
    end
  end

  def execute(
    %__MODULE__{},
    %HandleCompletedDownload{info_hash: info_hash}
  ) do
    %DownloadCompleted{info_hash: info_hash}
  end

  def apply(%__MODULE__{} = torrent, %DownloadStarted{} = event) do
    torrent
  end

  def apply(%__MODULE__{} = torrent, %TorrentAdded{} = event) do
    %__MODULE__{torrent |
      announce: event.announce,
      announce_list: event.announce_list,
      comment: event.comment,
      created_by: event.created_by,
      creation_date: event.creation_date,
      info: event.info,
      info_hash: event.info_hash
    }
  end

  def apply(%__MODULE__{} = torrent, %PieceHashSucceeded{} = event) do
    %__MODULE__{torrent |
      verified: IntSet.put(torrent.verified, event.index)
    }
  end

  def apply(%__MODULE__{} = torrent, %PieceHashFailed{info_hash: info_hash, index: index} = event) do
    %__MODULE__{torrent |
      pieces: Map.drop(torrent.pieces, index)
    }
  end

  def apply(%__MODULE__{} = torrent, %AllPiecesVerified{info_hash: info_hash}) do
    torrent
  end

  def apply(
    %__MODULE__{} = torrent,
    %BlockStored{pieces: pieces}
  ) do
    %__MODULE__{torrent |
      pieces: pieces
    }
  end

  def apply(%__MODULE__{} = torrent, %DownloadFailed{}) do
    %__MODULE__{torrent |
      pieces: []
    }
  end

  def apply(
    %__MODULE__{} = torrent,
    %DownloadCompleted{}
  ) do
    torrent
  end
end
