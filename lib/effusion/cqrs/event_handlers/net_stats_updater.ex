defmodule Effusion.CQRS.EventHandlers.NetStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.Messages
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.CQRS.Events.{
    PeerChokedUs,
    PeerHasBitfield,
    PeerHasPiece,
    PeerInterestedInUs,
    PeerCancelledRequest,
    PeerSentBlock,
    PeerSentHandshake,
    PeerUnchokedUs,
    PeerUninterestedInUs,
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }
  require Logger

  def handle(%PeerChokedUs{}, _metadata) do
    add_recv_bytes(:choke)
  end

  def handle(%PeerHasBitfield{bitfield: bitfield}, _metadata) do
    add_recv_bytes({:bitfield, Base.decode16!(bitfield)})
  end

  def handle(%PeerHasPiece{index: index}, _metadata) do
    add_recv_bytes({:have, index})
  end

  def handle(%PeerInterestedInUs{}, _metadata) do
    add_recv_bytes(:interested)
  end

  def handle(%PeerCancelledRequest{index: index, offset: offset, size: size}, _metadata) do
    add_recv_bytes({:cancel, index, offset, size})
  end

  def handle(%PeerSentBlock{index: index, offset: offset, data: data}, _metadata) do
    add_recv_bytes({:piece, index, offset, data})
  end

  def handle(%PeerSentHandshake{info_hash: info_hash, peer_id: peer_id}, _metadata) do
    add_recv_bytes({:handshake, peer_id, info_hash, []})
  end

  def handle(%PeerUnchokedUs{}, _metadata) do
    add_recv_bytes(:unchoke)
  end

  def handle(%PeerUninterestedInUs{}, _metadata) do
    add_recv_bytes(:uninterested)
  end

  def handle(
    %BitfieldSent{bitfield: bitfield},
    _metadata
  ) do
    {:ok, bitfield} = Base.decode16(bitfield)
    add_sent_bytes({:bitfield, bitfield})
  end

  def handle(
    %BlockRequested{index: index, offset: offset, size: size},
    _metadata
  ) do
    add_sent_bytes({:request, index, offset, size})
  end

  def handle(
    %InterestedSent{},
    _metadata
  ) do
    add_sent_bytes(:interested)
  end

  def handle(
    %RequestCancelled{index: index, offset: offset, size: size},
    _metadata
  ) do
    add_sent_bytes({:cancel, index, offset, size})
  end

  def handle(
    %SendingHave{index: index},
    _metadata
  ) do
    add_sent_bytes({:have, index})
  end

  defp add_sent_bytes(message) do
    {:ok, msg} = Messages.encode(message)
    NetStats.add_sent_bytes(byte_size(msg))
    :ok
  end

  defp add_recv_bytes(message) do
    {:ok, msg} = Messages.encode(message)
    NetStats.add_recv_bytes(byte_size(msg))
    :ok
  end

  def error(error, failed_event, _context) do
    Logger.error("NetStatsUpdater failed to process event #{inspect failed_event} due to error #{inspect error}")
    :skip
  end
end
