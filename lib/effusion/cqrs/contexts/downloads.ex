defmodule Effusion.CQRS.Contexts.Downloads do
  alias Effusion.CQRS.Commands.{
    AddTorrent,
    StartDownload,
    StoreBlock,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHandshake,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  }
  alias Effusion.CQRS.Application

  def add(meta) do
    %AddTorrent{
      announce: meta.announce,
      announce_list: Map.get(meta, :announce_list),
      comment: Map.get(meta, :comment),
      created_by: meta.created_by,
      info: meta.info,
      info_hash: Effusion.Hash.encode(meta.info_hash)
    }
    |> Application.dispatch()
  end

  def start(info_hash) do
    %StartDownload{
      info_hash: Effusion.Hash.encode(info_hash)
    }
    |> Application.dispatch()
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
    |> Application.dispatch()
  end

  def handle_message(info_hash, from, message) do
    info_hash = Effusion.Hash.encode(info_hash)
    case message do
      :choke -> %HandleChoke{info_hash: info_hash, peer_id: from}
      :unchoke -> %HandleUnchoke{info_hash: info_hash, peer_id: from}
      :interested -> %HandleInterested{info_hash: info_hash, peer_id: from}
      :uninterested -> %HandleUninterested{info_hash: info_hash, peer_id: from}
      {:have, index} -> %HandleHave{info_hash: info_hash, peer_id: from, index: index}
      {:bitfield, bitfield} -> %HandleBitfield{info_hash: info_hash, peer_id: from, bitfield: Base.encode16(bitfield)}
      {:request, block} -> %HandleRequest{info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, size: block.size}
      {:cancel, block} -> %HandleCancel{info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, size: block.size}
      {:piece, block} -> %HandlePiece{info_hash: info_hash, peer_id: from, index: block.index, offset: block.offset, data: block.data}
    end
    |> Application.dispatch()
  end
end
