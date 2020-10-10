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
    SendBitfield,
    SendHave
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
    status: :downloading,
    max_requests_per_peer: nil
  ]

  def interested?(%DownloadStarted{info_hash: info_hash}) do
    {:start!, info_hash}
  end

  def interested?(%PeerAdded{expected_info_hash: info_hash, from: "tracker"}) do
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
    %PeerAdded{peer_uuid: peer_uuid, from: "tracker"}
  ) do
    conn_count = Enum.count(connected_peers)
    half_open_count = Enum.count(connecting_to_peers)
    Logger.debug("**** Peer added, checking to see if we can connect. conn_count is #{conn_count} and half_open_count is #{half_open_count}")

    if (conn_count + half_open_count) < @max_connections and half_open_count < @max_half_open_connections do
      %AttemptToConnect{peer_uuid: peer_uuid}
    end
  end

  def handle(
    %__MODULE__{pieces: pieces},
    %PeerConnected{peer_uuid: peer_uuid}
  ) do
    %SendBitfield{peer_uuid: peer_uuid, bitfield: IntSet.bitstring(pieces)}
  end

  def handle(
    %__MODULE__{pieces: pieces},
    %PeerHasBitfield{peer_uuid: peer_uuid, bitfield: bitfield}
  ) do
    bitfield = bitfield |> Base.decode16!() |> IntSet.new()

    if IntSet.difference(bitfield, pieces) |> Enum.any?() do
      Logger.debug("***** Got bitfield, sending :interested")
      %SendInterested{
        peer_uuid: peer_uuid
      }
    end
  end

  def handle(
    %__MODULE__{
      info: info,
      target_piece_count: target_piece_count,
      pieces: pieces,
      blocks: blocks,
      block_size: block_size,
      requests: requests,
      max_requests_per_peer: max_requests_per_peer
    },
    %PeerUnchokedUs{peer_uuid: peer_uuid}
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

    existing_requests_to_this_peer =
      requests
      |> Enum.filter(fn {_ios, peers} -> Enum.member?(peers, peer_uuid) end)
      |> Enum.map(fn {ios, _peers} -> ios end)

    request_count_we_can_add =
      max_requests_per_peer - Enum.count(existing_requests_to_this_peer)

    blocks_to_request = Enum.take(required_blocks, request_count_we_can_add)

    Enum.map(blocks_to_request, fn {index, offset, size} ->
      %RequestBlock{
        peer_uuid: peer_uuid,
        index: index,
        offset: offset,
        size: size
      }
    end)
  end

  def handle(
    %__MODULE__{requests: requests},
    %PeerSentBlock{
      peer_uuid: from,
      info_hash: info_hash,
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
          peer_uuid: peer,
          index: index,
          offset: offset,
          size: byte_size(data)
        }
      end)

    [
      %StoreBlock{
        from: from,
        info_hash: info_hash,
        index: index,
        offset: offset,
        data: data
      } | cancellations
    ]
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers},
    %PieceHashSucceeded{index: index}
  ) do
    Logger.debug("****** Sending :have to all connected peers: #{inspect connected_peers}")
    Enum.map(connected_peers, fn peer_uuid ->
      %SendHave{peer_uuid: peer_uuid, index: index}
    end)
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers},
    %AllPiecesVerified{}
  ) do
    Logger.debug("***** All pieces verified, disconnecting all peers")

    Enum.map(connected_peers, fn peer_uuid ->
      %DisconnectPeer{
        peer_uuid: peer_uuid
      }
    end)
  end

  def handle(
    %__MODULE__{info_hash: info_hash, connected_peers: connected_peers, status: :shutting_down},
    %PeerDisconnected{}
  ) do
    if Enum.empty?(connected_peers) do
      %StopDownload{
        info_hash: info_hash,
        tracker_event: "stopped"
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
      bytes_left: bytes_left,
      max_requests_per_peer: max_requests_per_peer
    }
  ) do
    %__MODULE__{download |
      info_hash: info_hash,
      info: info,
      block_size: block_size,
      target_piece_count: Enum.count(info.pieces),
      announce: announce,
      annouce_list: announce_list,
      bytes_left: bytes_left,
      max_requests_per_peer: max_requests_per_peer
    }
  end

  def apply(
    %__MODULE__{connecting_to_peers: connecting_to_peers} = download,
    %AttemptingToConnect{peer_uuid: peer_uuid}
  ) do
    %__MODULE__{download |
      connecting_to_peers: MapSet.put(connecting_to_peers, peer_uuid)
    }
  end

  def apply(
    %__MODULE__{connecting_to_peers: connecting_to_peers, connected_peers: connected_peers} = download,
    %PeerConnected{peer_uuid: peer_uuid}
  ) do
    Logger.debug("****** peer connected #{peer_uuid}")

    %__MODULE__{download |
      connecting_to_peers: MapSet.delete(connecting_to_peers, peer_uuid),
      connected_peers: MapSet.put(connected_peers, peer_uuid)
    }
  end

  def apply(
    %__MODULE__{connected_peers: connected_peers, failcounts: failcounts} = download,
    %PeerDisconnected{peer_uuid: peer_uuid, reason: reason}
  ) do

    %__MODULE__{download |
      connected_peers: MapSet.delete(connected_peers, peer_uuid),
      failcounts: if(reason == :normal, do: failcounts, else: Map.update(failcounts, peer_uuid, -1, &(&1 - 1)))
    }
  end


  def apply(%__MODULE__{pieces: pieces, blocks: blocks} = download, %PieceHashSucceeded{index: index}) do
    %__MODULE__{download |
      pieces: IntSet.put(Base.decode16!(pieces), index) |> Base.encode16(),
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
    %BlockRequested{peer_uuid: peer_uuid, index: index, offset: offset, size: size}
  ) do
    %__MODULE__{download |
      requests: Map.update(requests, {index, offset, size}, MapSet.new([peer_uuid]), &MapSet.put(&1, peer_uuid))
    }
  end

  def apply(
    %__MODULE__{blocks: blocks} = download,
    %PeerSentBlock{
      index: index,
      offset: offset
    }
  ) do
    %__MODULE__{download |
      blocks: Map.update(blocks, index, IntSet.new(offset), &MapSet.put(&1, offset))
    }
  end

  defimpl Commanded.Serialization.JsonDecoder, for: Effusion.CQRS.ProcessManagers.DownloadTorrent do
    def decode(
      %Effusion.CQRS.ProcessManagers.DownloadTorrent{
        pieces: pieces,
        connecting_to_peers: connecting_to_peers,
        connected_peers: connected_peers
      } = state
    ) do
      %Effusion.CQRS.ProcessManagers.DownloadTorrent{state |
        pieces: IntSet.new(pieces),
        connecting_to_peers: MapSet.new(connecting_to_peers),
        connected_peers: MapSet.new(connected_peers)
      }
    end
  end
end
