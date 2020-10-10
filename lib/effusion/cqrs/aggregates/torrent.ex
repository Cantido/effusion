defmodule Effusion.CQRS.Aggregates.Torrent do
  alias Effusion.CQRS.Commands.{
    HandleCompletedDownload,
    AddTorrent,
    StartDownload,
    StopDownload,
    StoreBlock
  }
  alias Effusion.CQRS.Events.{
    BlockStored,
    AllPiecesVerified,
    DownloadStarted,
    DownloadStopped,
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
    bytes_downloaded: 0,
    bytes_uploaded: 0,
    bytes_left: nil,
    pieces: %{},
    verified: IntSet.new(),
    state: :stopped
  ]

  def execute(
    %__MODULE__{info_hash: nil},
    %AddTorrent{
      info_hash: info_hash,
      announce: announce,
      announce_list: announce_list,
      comment: comment,
      created_by: created_by,
      creation_date: creation_date,
      info: info
    }
  ) do
    %TorrentAdded{
      announce: announce,
      announce_list: announce_list,
      comment: comment,
      created_by: created_by,
      creation_date: creation_date,
      info: info,
      info_hash: info_hash
    }
  end

  def execute(%__MODULE__{}, %AddTorrent{}) do
    {:error, :torrent_already_exists}
  end

  def execute(
    %__MODULE__{
      info_hash: info_hash,
      verified: verified,
      info: info,
      announce: announce,
      announce_list: announce_list,
      state: :stopped
    },
    %StartDownload{block_size: block_size}
  ) do
    bytes_left = info.length - (Enum.count(verified) * info.piece_length)

    %DownloadStarted{
      info_hash: info_hash,
      announce: announce,
      bytes_left: bytes_left,
      announce_list: announce_list,
      bytes_uploaded: 0, # TODO
      bytes_downloaded: 0, # TODO
      info: info,
      block_size: block_size
    }
  end

  def execute(
    %__MODULE__{state: :downloading},
    %StartDownload{}
  ) do
    {:error, :download_already_downloading}
  end

  def execute(
    %__MODULE__{state: :completed},
    %StartDownload{}
  ) do
    {:error, :download_completed}
  end

  def execute(
    %__MODULE__{
      info_hash: info_hash,
      announce: announce,
      announce_list: announce_list,
      comment: comment,
      created_by: created_by,
      creation_date: creation_date,
      bytes_left: bytes_left,
      bytes_downloaded: bytes_downloaded,
      bytes_uploaded: bytes_uploaded,
      state: :downloading
    } = torrent,
    %StopDownload{tracker_event: tracker_event}
  ) do

    %DownloadStopped{
      info_hash: info_hash,
      announce: announce,
      announce_list: announce_list,
      comment: comment,
      created_by: created_by,
      creation_date: creation_date,
      info: torrent.info,
      bytes_downloaded: bytes_downloaded,
      bytes_uploaded: bytes_uploaded,
      bytes_left: bytes_left,
      tracker_event: tracker_event
    }
  end

  def execute(
    %__MODULE__{state: :stopped},
    %StopDownload{}
  ) do
    {:error, :download_already_stopped}
  end

  def execute(
    %__MODULE__{state: :completed},
    %StopDownload{}
  ) do
    {:error, :download_completed}
  end

  def execute(
    %__MODULE__{} = torrent,
    %StoreBlock{from: from, index: index, offset: offset, data: block_data}
  ) do
    torrent
    |> Multi.new()
    |> Multi.execute(&store_block(&1, from, index, offset, block_data))
    |> Multi.execute(&check_for_finished_piece(&1, index))
    |> Multi.execute(&check_for_finished_torrent/1)
  end

  def execute(
    %__MODULE__{},
    %HandleCompletedDownload{info_hash: info_hash}
  ) do
    %DownloadCompleted{info_hash: info_hash}
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

      if Effusion.Hash.calc(piece_data) == Effusion.Hash.decode(Enum.at(info.pieces, index)) do
        %PieceHashSucceeded{
          info_hash: info_hash,
          index: index,
          data: piece_data,
          info: info
        }
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

  def apply(%__MODULE__{} = torrent, %DownloadStarted{}) do
    %__MODULE__{torrent|
      state: :downloading
    }
  end

  def apply(%__MODULE__{} = torrent, %DownloadCompleted{}) do
    %__MODULE__{torrent|
      state: :completed
    }
  end

  def apply(%__MODULE__{} = torrent, %DownloadStopped{}) do
    %__MODULE__{torrent|
      state: :stopped
    }
  end

  def apply(%__MODULE__{} = torrent, %TorrentAdded{} = event) do
    %__MODULE__{torrent |
      announce: event.announce,
      announce_list: event.announce_list,
      comment: event.comment,
      created_by: event.created_by,
      creation_date: event.creation_date,
      info: event.info,
      info_hash: event.info_hash,
      bytes_left: event.info.length
    }
  end

  def apply(%__MODULE__{bytes_left: bytes_left, info: info} = torrent, %PieceHashSucceeded{index: index}) do
    piece_size = if index == (Enum.count(info.pieces) - 1) do
      # This is the last piece of the torrent, so it is smaller
      expected_piece_count = Enum.count(info.pieces)
      info.length - (expected_piece_count - 1) * info.piece_length
    else
      info.piece_length
    end

    %__MODULE__{torrent |
      verified: IntSet.put(torrent.verified, index),
      bytes_left: bytes_left - piece_size
    }
  end

  def apply(%__MODULE__{} = torrent, %PieceHashFailed{index: index}) do
    %__MODULE__{torrent |
      pieces: Map.drop(torrent.pieces, index)
    }
  end

  def apply(%__MODULE__{} = torrent, %AllPiecesVerified{}) do
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

  defimpl Commanded.Serialization.JsonDecoder, for: Effusion.CQRS.Aggregates.Torrent do
    def decode(
      %Effusion.CQRS.Aggregates.Torrent{
        verified: verified
      } = state
    ) do
      %Effusion.CQRS.Aggregates.Torrent{state |
        verified: IntSet.new(verified)
      }
    end
  end
end
