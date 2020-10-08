defmodule Effusion.CQRS.ProcessManagers.DownloadTorrent do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.CQRS.Commands.{
    AttemptToConnect,
    DisconnectPeer,
    StopDownload,
    StoreBlock,
    SendInterested,
    RequestBlock,
    CancelRequest,
    SendBitfield
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    DownloadStarted,
    DownloadStopped,
    DownloadCompleted,
    PieceHashSucceeded,
    PeerAdded,
    PeerConnected,
    PeerDisconnected,
    PeerHasBitfield,
    PeerSentBlock,
    PeerUnchokedUs,
    AllPiecesVerified,
    BlockRequested
  }
  require Logger

  @max_half_open_connections 500
  @max_connections Application.fetch_env!(:effusion, :max_peers)

  @derive Jason.Encoder
  defstruct [
    info_hash: nil,
    target_piece_count: nil,
    info: nil,
    block_size: nil,
    pieces: IntSet.new(),
    blocks: Map.new(),
    announce: nil,
    annouce_list: [],
    bytes_uploaded: 0,
    bytes_downloaded: 0,
    bytes_left: nil,
    connecting_to_peers: MapSet.new(),
    connected_peers: MapSet.new(),
    failcounts: Map.new(),
    requests: Map.new(),
    status: :downloading
  ]

  def interested?(%DownloadStarted{info_hash: info_hash}) do
    {:start!, info_hash}
  end

  def interested?(%PeerAdded{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%AttemptingToConnect{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerConnected{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerDisconnected{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerHasBitfield{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerUnchokedUs{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerSentBlock{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PieceHashSucceeded{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%AllPiecesVerified{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%DownloadStopped{info_hash: info_hash}) do
    {:stop, info_hash}
  end

  def interested?(%DownloadCompleted{info_hash: info_hash}) do
    {:stop, info_hash}
  end

  def interested?(_) do
    false
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers, connecting_to_peers: connecting_to_peers},
    %PeerAdded{internal_peer_id: internal_peer_id}
  ) do
    conn_count = Enum.count(connected_peers)
    half_open_count = Enum.count(connecting_to_peers)

    if (conn_count + half_open_count) < @max_connections and half_open_count < @max_half_open_connections do
      %AttemptToConnect{internal_peer_id: internal_peer_id}
    end
  end
  def handle(
    %__MODULE__{pieces: pieces},
    %PeerConnected{internal_peer_id: internal_peer_id}
  ) do
    %SendBitfield{internal_peer_id: internal_peer_id, bitfield: IntSet.bitstring(pieces)}
  end

  def handle(
    %__MODULE__{pieces: pieces},
    %PeerHasBitfield{internal_peer_id: internal_peer_id, bitfield: bitfield}
  ) do
    bitfield = bitfield |> Base.decode16!() |> IntSet.new()

    if IntSet.difference(bitfield, pieces) |> Enum.any?() do
      Logger.debug("***** Got bitfield, sending :interested")
      %SendInterested{
        internal_peer_id: internal_peer_id
      }
    end
  end

  def handle(
    %__MODULE__{
      info: info,
      target_piece_count: target_piece_count,
      pieces: pieces,
      blocks: blocks,
      block_size: block_size
    },
    %PeerUnchokedUs{internal_peer_id: internal_peer_id}
  ) do
    Logger.debug("***** Got :unchoke, sending :request")
    blocks_per_piece =
      if block_size < info.piece_length do
        trunc(info.piece_length / block_size)
      else
        1
      end

    required_blocks =
      pieces # all pieces we have
      |> IntSet.inverse(target_piece_count) # all pieces we need
      |> Enum.flat_map(fn required_piece_index ->
        blocks
        |> Map.get(required_piece_index, IntSet.new()) # all blocks we have in this piece index
        |> IntSet.inverse(blocks_per_piece) # all blocks we need in this piece index
        |> Enum.map(fn required_block_offset ->
          {required_piece_index, required_block_offset, block_size} # the block we need to request
        end)
      end)

    Enum.map(required_blocks, fn {index, offset, size} ->
      %RequestBlock{
        internal_peer_id: internal_peer_id,
        index: index,
        offset: offset,
        size: size
      }
    end)
  end

  def handle(
    %__MODULE__{requests: requests},
    %PeerSentBlock{
      info_hash: info_hash,
      peer_id: peer_id,
      index: index,
      offset: offset,
      data: data
    }
  ) do
    Logger.debug("***** Got block, storing it, BTW requests is #{inspect requests}")

    cancellations =
      requests
      |> Map.get({index, offset, byte_size(data)}, MapSet.new())
      |> Enum.map(fn peer ->
        %CancelRequest{
          internal_peer_id: peer,
          index: index,
          offset: offset,
          size: byte_size(data)
        }
      end)

    [
      %StoreBlock{
        info_hash: info_hash,
        from: peer_id,
        index: index,
        offset: offset,
        data: data
      } | cancellations
    ]
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers},
    %AllPiecesVerified{}
  ) do
    Logger.debug("***** All pieces verified, disconnecting all peers")

    Enum.map(connected_peers, fn internal_peer_id ->
      %DisconnectPeer{
        internal_peer_id: internal_peer_id
      }
    end)
  end

  def handle(
    %__MODULE__{info_hash: info_hash, connected_peers: connected_peers, status: :shutting_down},
    %PeerDisconnected{}
  ) do
    if Enum.empty?(connected_peers) do
      %StopDownload{
        info_hash: info_hash
      }
    end
  end

  def apply(
    %__MODULE__{info_hash: nil} = download,
    %DownloadStarted{
      info_hash: info_hash,
      info: info,
      block_size: block_size,
      announce: announce,
      announce_list: announce_list,
      bytes_left: bytes_left
    }
  ) do
    %__MODULE__{download |
      info_hash: info_hash,
      info: info,
      block_size: block_size,
      target_piece_count: Enum.count(info.pieces),
      announce: announce,
      annouce_list: announce_list,
      bytes_left: bytes_left
    }
  end

  def apply(
    %__MODULE__{connecting_to_peers: connecting_to_peers} = download,
    %AttemptingToConnect{internal_peer_id: internal_peer_id}
  ) do
    %__MODULE__{download |
      connecting_to_peers: MapSet.put(connecting_to_peers, internal_peer_id)
    }
  end

  def apply(
    %__MODULE__{connecting_to_peers: connecting_to_peers, connected_peers: connected_peers} = download,
    %PeerConnected{internal_peer_id: internal_peer_id, initiated_by: :us}
  ) do

    %__MODULE__{download |
      connecting_to_peers: MapSet.delete(connecting_to_peers, internal_peer_id),
      connected_peers: MapSet.put(connected_peers, internal_peer_id)
    }
  end

  def apply(
    %__MODULE__{connected_peers: connected_peers, failcounts: failcounts} = download,
    %PeerDisconnected{internal_peer_id: internal_peer_id, reason: reason}
  ) do

    %__MODULE__{download |
      connected_peers: MapSet.delete(connected_peers, internal_peer_id),
      failcounts: if(reason == :normal, do: failcounts, else: Map.update(failcounts, internal_peer_id, -1, &(&1 - 1)))
    }
  end


  def apply(%__MODULE__{pieces: pieces, blocks: blocks} = download, %PieceHashSucceeded{index: index}) do
    %__MODULE__{download |
      pieces: IntSet.put(pieces, index),
      blocks: Map.put(blocks, index, :complete)
    }
  end


  def apply(download, %AllPiecesVerified{}) do
    %__MODULE__{download |
      status: :shutting_down
    }
  end

  def apply(
    %__MODULE__{requests: requests} = download,
    %BlockRequested{internal_peer_id: internal_peer_id, index: index, offset: offset, size: size}
  ) do
    %__MODULE__{download |
      requests: Map.update(requests, {index, offset, size}, MapSet.new([internal_peer_id]), &MapSet.put(&1, internal_peer_id))
    }
  end

  def apply(
    %__MODULE__{blocks: blocks} = download,
    %PeerSentBlock{internal_peer_id: internal_peer_id, index: index, offset: offset, data: data}
  ) do
    %__MODULE__{download |
      blocks: Map.update(blocks, index, IntSet.new(offset), &MapSet.put(&1, offset))
    }
  end
end
