defmodule Effusion.CQRS.Contexts.Downloads do
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    StartDownload,
    StopDownload,
    StoreBlock,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  }
  alias Effusion.CQRS.Application, as: CQRS

  def add(meta) do
    %AddTorrent{
      announce: meta.announce,
      announce_list: Map.get(meta, :announce_list),
      comment: Map.get(meta, :comment),
      created_by: meta.created_by,
      info: meta.info,
      info_hash: Effusion.Hash.encode(meta.info_hash)
    }
    |> CQRS.dispatch()
  end

  def start(info_hash, block_size) do
    %StartDownload{
      info_hash: Effusion.Hash.encode(info_hash),
      block_size: block_size
    }
    |> CQRS.dispatch()
  end

  def stop(info_hash) do
    %StopDownload{
      info_hash: Effusion.Hash.encode(info_hash),
      tracker_event: "stopped"
    }
    |> CQRS.dispatch(consistency: :strong)
  end

  def store_block(info_hash, from, index, offset, data)
    when is_number(index)
    and is_number(offset)
  do
    %StoreBlock{
      from: from,
      info_hash: Effusion.Hash.encode(info_hash),
      index: index,
      offset: offset,
      data: data
    }
    |> CQRS.dispatch()
  end

  def handle_message(info_hash, from, host, port, message) do
    info_hash = Effusion.Hash.encode(info_hash)
    host = to_string(:inet.ntoa(host))
    internal_peer_id = "#{info_hash}:#{host}:#{port}"
    case message do
      :choke -> %HandleChoke{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from}
      :unchoke -> %HandleUnchoke{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from}
      :interested -> %HandleInterested{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from}
      :uninterested -> %HandleUninterested{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from}
      {:have, index} -> %HandleHave{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from, index: index}
      {:bitfield, bitfield} -> %HandleBitfield{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from, bitfield: Base.encode16(bitfield)}
      {:request, block} -> %HandleRequest{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, size: block.size}
      {:cancel, block} -> %HandleCancel{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, size: block.size}
      {:piece, block} -> %HandlePiece{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, data: block.data}
    end
    |> CQRS.dispatch()
  end
end
