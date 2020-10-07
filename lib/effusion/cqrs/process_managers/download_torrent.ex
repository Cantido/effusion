defmodule Effusion.CQRS.ProcessManagers.DownloadTorrent do
  use Commanded.ProcessManagers.ProcessManager,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.Connection
  alias Effusion.CQRS.Commands.{
    AddPeer,
    HandleCompletedDownload,
    StoreBlock
  }
  alias Effusion.CQRS.Events.{
    DownloadStarted,
    DownloadCompleted,
    PieceHashSucceeded,
    PeerAdded,
    PeerHasBitfield,
    PeerSentBlock,
    PeerUnchokedUs,
    AllPiecesVerified
  }
  require Logger

  @max_half_open_connections 500
  @max_connections Application.fetch_env!(:effusion, :max_peers)

  @derive Jason.Encoder
  defstruct [
    info_hash: nil,
    target_piece_count: nil,
    pieces: IntSet.new(),
    connecting_count: 0,
    connected_count: 0,
    announce: nil,
    annouce_list: [],
    bytes_uploaded: 0,
    bytes_downloaded: 0,
    bytes_left: nil
  ]

  def interested?(%DownloadStarted{info_hash: info_hash}) do
    {:start!, info_hash}
  end

  def interested?(%PeerAdded{info_hash: info_hash}) do
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

  def interested?(%DownloadCompleted{info_hash: info_hash}) do
    {:stop, info_hash}
  end

  def interested?(_) do
    false
  end

  def handle(
    download,
    %DownloadStarted{
      info_hash: info_hash,
      announce: announce_uri,
      bytes_left: bytes_left
    }
  ) do
    thp_client  = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)
    tracker_numwant = Application.fetch_env!(:effusion, :max_peers)

    opts = [event: "started", numwant: tracker_numwant]

    {:ok, res} = thp_client.announce(
      announce_uri,
      local_host,
      local_port,
      peer_id,
      info_hash,
      0,
      0,
      bytes_left,
      opts
    )

    Enum.map(res.peers, fn peer ->
      %AddPeer{
        info_hash: info_hash,
        host: :inet.ntoa(peer.ip),
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil),
        from: :tracker
      }
    end)
    |> Enum.filter(fn peer ->
      peer.port > 0
    end)
  end

  def handle(
    download,
    %PeerHasBitfield{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  ) do
    Logger.debug("***** Got bitfield, sending :interested")
    ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, :interested)
    []
  end

  def handle(
    download,
    %PieceHashSucceeded{info_hash: info_hash, index: index}
  ) do
    Logger.debug("***** Piece has succeeded, sending :have")
    ConnectionRegistry.btp_broadcast(Effusion.Hash.decode(info_hash), {:have, index})
    []
  end

  def handle(
    %__MODULE__{target_piece_count: target_piece_count, pieces: pieces},
    %PeerUnchokedUs{info_hash: info_hash, peer_id: peer_id}
  ) do
    Logger.debug("***** Got :unchoke, sending :request")
    block_size = Application.fetch_env!(:effusion, :block_size)

    required_pieces = pieces |> IntSet.inverse(target_piece_count)

    Enum.each(required_pieces, fn piece_to_request ->
      ConnectionRegistry.btp_send(Effusion.Hash.decode(info_hash), peer_id, {:request, piece_to_request, 0, block_size})
    end)
    []
  end

  def handle(
    %__MODULE__{},
    %PeerSentBlock{info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, data: data}
  ) do
    Logger.debug("***** Got block, storing it")
    %StoreBlock{info_hash: info_hash, from: peer_id, index: index, offset: offset, data: data}
  end

  def handle(
    %__MODULE__{announce: announce},
    %AllPiecesVerified{info_hash: info_hash}
  ) do
    Logger.debug("***** All pieces verified, contacting tracker")

    thp_client  = Application.fetch_env!(:effusion, :thp_client)
    peer_id = Application.fetch_env!(:effusion, :peer_id)
    {local_host, local_port} = Application.fetch_env!(:effusion, :server_address)
    tracker_numwant = Application.fetch_env!(:effusion, :max_peers)

    opts = [event: "completed", numwant: 0]

    {:ok, res} = thp_client.announce(
      announce,
      local_host,
      local_port,
      peer_id,
      info_hash,
      0,
      0,
      0,
      opts
    )

    peers =
      Enum.map(res.peers, fn peer ->
        %AddPeer{
          info_hash: info_hash,
          host: peer.ip,
          port: peer.port,
          peer_id: Map.get(peer, :peer_id, nil),
          from: :tracker
        }
      end)
      |> Enum.filter(fn peer ->
        peer.port > 0
      end)

    peers ++ [%HandleCompletedDownload{info_hash: info_hash}]
  end



  def apply(
    %__MODULE__{info_hash: nil} = download,
    %DownloadStarted{info_hash: info_hash, info: info, announce: announce, announce_list: announce_list, bytes_left: bytes_left}
  ) do
    %__MODULE__{download |
      info_hash: info_hash,
      target_piece_count: Enum.count(info.pieces),
      announce: announce,
      annouce_list: announce_list,
      bytes_left: bytes_left
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, connecting_count: connecting_count, connected_count: connected_count} = download,
    %PeerAdded{info_hash: info_hash, peer_id: peer_id, host: host, port: port, from: :tracker}
  ) do

    if connecting_count < @max_half_open_connections and connected_count < @max_connections do
      Logger.debug "**** CQRS is opening a connection to #{host}:#{port}"
      {:ok, host} = :inet.parse_address(host |> String.to_charlist())
      info_hash = Effusion.Hash.decode(info_hash)
      Connection.connect({{host, port}, info_hash, peer_id})
    else
      Logger.debug "**** CQRS is deciding not to open a connection to #{host}:#{port}"
    end

    %__MODULE__{download |
      connecting_count: connecting_count + 1
    }
  end


  def apply(
    download,
    %PieceHashSucceeded{info_hash: info_hash, index: index}
  ) do
    %__MODULE__{download |
      pieces: IntSet.put(download.pieces, index)
    }
  end
end
