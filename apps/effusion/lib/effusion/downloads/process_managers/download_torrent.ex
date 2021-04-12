defmodule Effusion.Downloads.ProcessManagers.DownloadTorrent do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.Connection.Commands.{
    AttemptToConnect,
    DisconnectPeer
  }
  alias Effusion.PWP.Messages.Outgoing.Commands.{
    CancelRequest,
    RequestBlock,
    SendBitfield,
    SendHave,
    SendInterested
  }
  alias Effusion.Downloads.Commands.{
    StopDownload,
    StoreBlock,
  }
  alias Effusion.PWP.Messages.Outgoing.Events.BlockRequested
  alias Effusion.PWP.Messages.Incoming.Events.{
    PeerHasBitfield,
    PeerHasPiece,
    PeerSentBlock,
    PeerUnchokedUs
  }
  alias Effusion.Downloads.Events.{
    DownloadStarted,
    DownloadStopped,
    DownloadCompleted,
    PieceHashSucceeded,
    AllPiecesVerified
  }
  alias Effusion.PWP.Connection.Events.{
    AttemptingToConnect,
    ConnectionAttemptFailed,
    PeerConnected,
    PeerDisconnected
  }
  alias Effusion.PWP.Swarm.Events.PeerAdded
  require Logger

  @derive Jason.Encoder
  defstruct [
    info_hash: nil,
    target_piece_count: nil,
    info: nil,
    block_size: nil,
    pieces: IntSet.new(),
    peer_bitfields: Map.new(),
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
    status: "downloading",
    max_requests_per_peer: nil,
    max_half_open_connections: nil,
    max_connections: nil
  ]

  def interested?(%DownloadStarted{info_hash: info_hash}) do
    {:start!, info_hash}
  end

  def interested?(%PeerAdded{expected_info_hash: info_hash, from: "tracker"}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerAdded{expected_info_hash: info_hash, from: "dht"}) do
    {:continue!, info_hash}
  end

  def interested?(%AttemptingToConnect{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%ConnectionAttemptFailed{info_hash: info_hash}) do
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

  def interested?(%PeerHasPiece{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerUnchokedUs{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%PeerSentBlock{info_hash: info_hash}) do
    {:continue!, info_hash}
  end

  def interested?(%BlockRequested{info_hash: info_hash}) do
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
    %__MODULE__{} = download,
    %PeerAdded{peer_uuid: peer_uuid, from: "tracker"}
  ) do
    if attempt_to_connect_to_new_peers?(download) do
      %AttemptToConnect{peer_uuid: peer_uuid}
    end
  end

  def handle(
    %__MODULE__{} = download,
    %PeerAdded{peer_uuid: peer_uuid, from: "dht"}
  ) do
    Logger.debug("********* DownloadTorrent got the peer")
    if attempt_to_connect_to_new_peers?(download) do
      %AttemptToConnect{peer_uuid: peer_uuid}
    end
  end

  def handle(
    %__MODULE__{} = download,
    %ConnectionAttemptFailed{} = event
  ) do
    download = __MODULE__.apply(download, event)
    unless at_connection_limit?(download) do
      if best_peer = next_peer_connection(download) do
        %AttemptToConnect{peer_uuid: best_peer}
      end
    end
  end

  def handle(
    %__MODULE__{pieces: pieces},
    %PeerConnected{peer_uuid: peer_uuid}
  ) do
    encoded_bitfield = IntSet.bitstring(pieces, byte_align: true) |> Base.encode64()
    %SendBitfield{peer_uuid: peer_uuid, bitfield: encoded_bitfield}
  end

  def handle(
    %__MODULE__{pieces: pieces},
    %PeerHasBitfield{peer_uuid: peer_uuid, bitfield: bitfield}
  ) do
    bitfield = bitfield |> Base.decode64!() |> IntSet.new()

    if IntSet.difference(bitfield, pieces) |> Enum.any?() do
      Logger.debug("***** Got bitfield, sending :interested")
      %SendInterested{
        peer_uuid: peer_uuid
      }
    end
  end

  def handle(
    %__MODULE__{} = download,
    %PeerUnchokedUs{peer_uuid: peer_uuid}
  ) do
    Logger.debug("***** Got :unchoke, sending :request")
    get_more_requests_for_peer(download, peer_uuid)
  end

  def handle(
    %__MODULE__{block_size: block_size} = download,
    %PeerSentBlock{
      peer_uuid: from,
      info_hash: info_hash,
      index: index,
      offset: offset,
      data: data
    } = event
  ) do
    # `handle/2` is called before `apply/2` in commanded (that will change someday, hopefully)
    download = __MODULE__.apply(download, event)
    requests = download.requests

    Logger.debug("***** Got block #{inspect {index, offset}}, storing it, BTW requests is #{inspect requests}")

    cancellations =
      requests
      |> Map.get({index, offset, block_size}, MapSet.new())
      |> Enum.map(fn peer ->
        %CancelRequest{
          peer_uuid: peer,
          index: index,
          offset: offset,
          size: block_size
        }
      end)

    new_requests = get_more_requests_for_peer(download, from)

    [
      %StoreBlock{
        from: from,
        info_hash: info_hash,
        index: index,
        offset: offset,
        data: data
      }
    ] ++ cancellations ++ new_requests
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers},
    %PieceHashSucceeded{index: index}
  ) do
    Enum.map(connected_peers, fn peer_uuid ->
      %SendHave{peer_uuid: peer_uuid, index: index}
    end)
  end

  def handle(
    %__MODULE__{connected_peers: connected_peers},
    %AllPiecesVerified{}
  ) do
    Enum.map(connected_peers, fn peer_uuid ->
      %DisconnectPeer{
        peer_uuid: peer_uuid
      }
    end)
  end

  def handle(
    %__MODULE__{info_hash: info_hash, connected_peers: connected_peers, status: "shutting down"},
    %PeerDisconnected{}
  ) do
    if Enum.empty?(connected_peers) do
      %StopDownload{
        info_hash: info_hash,
        tracker_event: "stopped"
      }
    end
  end

  def handle(
    %__MODULE__{status: "downloading"} = download,
    %PeerDisconnected{}
  ) do
    if best_peer = next_peer_connection(download) do
      %AttemptToConnect{peer_uuid: best_peer}
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
      max_requests_per_peer: max_requests_per_peer,
      max_half_open_connections: max_half_open_connections,
      max_connections: max_connections
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
      max_requests_per_peer: max_requests_per_peer,
      max_half_open_connections: max_half_open_connections,
      max_connections: max_connections
    }
  end

  def apply(
    %__MODULE__{failcounts: failcounts, connecting_to_peers: connecting_to_peers} = download,
    %PeerAdded{peer_uuid: peer_uuid}
  ) do
    Logger.debug("********** peer added, seeing if we should connect")
    connecting_to_peers = if attempt_to_connect_to_new_peers?(download) do
      MapSet.put(connecting_to_peers, peer_uuid)
    else
      connecting_to_peers
    end

    %__MODULE__{download |
      failcounts: Map.put(failcounts, peer_uuid, 0),
      connecting_to_peers: connecting_to_peers
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
    %__MODULE__{connecting_to_peers: connecting_to_peers, failcounts: failcounts} = download,
    %ConnectionAttemptFailed{peer_uuid: peer_uuid, reason: reason}
  ) do
    %__MODULE__{download |
      connecting_to_peers: MapSet.delete(connecting_to_peers, peer_uuid),
      failcounts: if(reason == "normal", do: failcounts, else: Map.update(failcounts, peer_uuid, 1, &(&1 + 1)))
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
      failcounts: if(reason == "normal", do: failcounts, else: Map.update(failcounts, peer_uuid, 1, &(&1 + 1)))
    }
  end

  def apply(
    %__MODULE__{peer_bitfields: peer_bitfields} = download,
    %PeerHasBitfield{peer_uuid: peer_uuid, bitfield: bitfield}
  ) do
    %__MODULE__{download |
      peer_bitfields: Map.put(peer_bitfields, peer_uuid, Base.decode64!(bitfield) |> IntSet.new())
    }
  end

  def apply(
    %__MODULE__{peer_bitfields: peer_bitfields} = download,
    %PeerHasPiece{peer_uuid: peer_uuid, index: index}
  ) do
    %__MODULE__{download |
      peer_bitfields: Map.update(peer_bitfields, peer_uuid, IntSet.new([index]), &IntSet.put(&1, index))
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
      status: "shutting down"
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
    %__MODULE__{
      blocks: blocks,
      requests: requests
    } = download,
    %PeerSentBlock{
      peer_uuid: peer_uuid,
      index: index,
      offset: offset,
      data: data
    }
  ) do
    size =
      data
      |> Base.decode64!()
      |> byte_size()
    %__MODULE__{download |
      blocks: Map.update(blocks, index, IntSet.new(offset), &IntSet.put(&1, offset)),
      requests: Map.update(requests, {index, offset, size}, MapSet.new(), &MapSet.delete(&1, peer_uuid))
    }
  end

  defp attempt_to_connect_to_new_peers?(download) do
    not at_connection_limit?(download)
  end

  defp at_connection_limit?(
    %__MODULE__{
      connected_peers: connected_peers,
      connecting_to_peers: connecting_to_peers,
      max_connections: max_connections,
      max_half_open_connections: max_half_open_connections
    }
  ) do
    conn_count = Enum.count(connected_peers)
    half_open_count = Enum.count(connecting_to_peers)

    (conn_count + half_open_count) >= max_connections or half_open_count >= max_half_open_connections
  end

  defp next_peer_connection(
    %__MODULE__{
      connected_peers: connected_peers,
      connecting_to_peers: connecting_to_peers,
      failcounts: failcounts
    }
  ) do
    best_potential_peers =
      failcounts
      |> Enum.reject(fn {peer_uuid, _failcount} ->
        Enum.member?(connected_peers, peer_uuid)
      end)
      |> Enum.reject(fn {peer_uuid, _failcount} ->
        Enum.member?(connecting_to_peers, peer_uuid)
      end)

    if Enum.empty?(best_potential_peers) do
      nil
    else
      Enum.max_by(best_potential_peers, &elem(&1, 1))
      |> elem(0)
    end
  end

  defp get_more_requests_for_peer(
    %__MODULE__{
      info: info,
      target_piece_count: target_piece_count,
      pieces: pieces,
      blocks: blocks,
      block_size: block_size,
      requests: requests,
      peer_bitfields: peer_bitfields,
      max_requests_per_peer: max_requests_per_peer
    },
    peer_uuid
  ) do
    blocks_per_piece =
      if block_size < info.piece_length do
        trunc(info.piece_length / block_size)
      else
        1
      end

    block_size = min(block_size, info.piece_length)

    pieces_peer_has = Map.get(peer_bitfields, peer_uuid, IntSet.new())

    existing_requests_to_this_peer =
      requests
      |> Enum.filter(fn {_ios, peers} -> Enum.member?(peers, peer_uuid) end)
      |> Enum.map(fn {ios, _peers} -> ios end)
      |> MapSet.new()

    request_count_we_can_add =
      max_requests_per_peer - Enum.count(existing_requests_to_this_peer)

    blocks_to_request =
      pieces # all pieces we have
      |> IntSet.inverse(target_piece_count) # all pieces we need
      |> Stream.flat_map(fn required_piece_index ->
        blocks
        |> Map.get(required_piece_index, IntSet.new()) # all blocks we have in this piece index
        |> IntSet.inverse(blocks_per_piece) # all blocks we need in this piece index
        |> Stream.map(fn required_block_index ->
          {required_piece_index, required_block_index * block_size, block_size} # the block we need to request
        end)
      end)
      |> Stream.filter(fn {i, _o, _s} -> Enum.member?(pieces_peer_has, i) end) # All requests for block that the peer has
      |> Stream.reject(&Enum.member?(existing_requests_to_this_peer, &1)) # All requests for blocks that the peer has that we haven't already requested
      |> Enum.take(request_count_we_can_add)

    Enum.map(blocks_to_request, fn {index, offset, size} ->
      %RequestBlock{
        peer_uuid: peer_uuid,
        index: index,
        offset: offset,
        size: size
      }
    end)
  end

  defimpl Commanded.Serialization.JsonDecoder, for: Effusion.CQRS.ProcessManagers.DownloadTorrent do
    def decode(
      %Effusion.Downloads.ProcessManagers.DownloadTorrent{
        pieces: pieces,
        connecting_to_peers: connecting_to_peers,
        connected_peers: connected_peers,
        peer_bitfields: peer_bitfields
      } = state
    ) do
      %Effusion.Downloads.ProcessManagers.DownloadTorrent{state |
        pieces: IntSet.new(Base.decode64!(pieces)),
        connecting_to_peers: MapSet.new(connecting_to_peers),
        connected_peers: MapSet.new(connected_peers),
        peer_bitfields: Enum.map(peer_bitfields, fn {uuid, bitfield} -> {uuid, Base.decode64!(bitfield) |> IntSet.new()} end)
      }
    end
  end
end
