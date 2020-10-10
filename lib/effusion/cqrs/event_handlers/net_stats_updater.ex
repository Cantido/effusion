defmodule Effusion.CQRS.EventHandlers.NetStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.PWP.Messages
  alias Effusion.Statistics.Net, as: NetStats
  alias Effusion.CQRS.Events.{
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }

  def handle(
    %BitfieldSent{bitfield: bitfield},
    _metadata
  ) do
    bitfield = Base.decode16(bitfield)
    add_message_bytes({:bitfield, bitfield})
  end

  def handle(
    %BlockRequested{index: index, offset: offset, size: size},
    _metadata
  ) do
    add_message_bytes({:request, index, offset, size})
  end

  def handle(
    %InterestedSent{},
    _metadata
  ) do
    add_message_bytes(:interested)
  end

  def handle(
    %RequestCancelled{index: index, offset: offset, size: size},
    _metadata
  ) do
    add_message_bytes({:cancel, index, offset, size})
  end

  def handle(
    %SendingHave{index: index},
    _metadata
  ) do
    add_message_bytes({:have, index})
  end

  defp add_message_bytes(message) do
    {:ok, msg} = Message.encode(message)
    NetStats.add_sent_bytes(byte_size(msg))
  end
end
