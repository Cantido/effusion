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

  def handle_message(peer_uuid, message) do
    case message do
      :choke -> %HandleChoke{peer_uuid: peer_uuid}
      :unchoke -> %HandleUnchoke{peer_uuid: peer_uuid}
      :interested -> %HandleInterested{peer_uuid: peer_uuid}
      :uninterested -> %HandleUninterested{peer_uuid: peer_uuid}
      {:have, index} -> %HandleHave{peer_uuid: peer_uuid, index: index}
      {:bitfield, bitfield} -> %HandleBitfield{peer_uuid: peer_uuid, bitfield: Base.encode16(bitfield)}
      {:request, block} -> %HandleRequest{peer_uuid: peer_uuid, index: block.index, offset: block.offset, size: block.size}
      {:cancel, block} -> %HandleCancel{peer_uuid: peer_uuid, index: block.index, offset: block.offset, size: block.size}
      {:piece, block} -> %HandlePiece{peer_uuid: peer_uuid, index: block.index, offset: block.offset, data: block.data}
    end
    |> CQRS.dispatch()
  end
end
