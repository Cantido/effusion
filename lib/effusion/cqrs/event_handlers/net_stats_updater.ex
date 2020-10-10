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
  require Logger

  def handle(
    %BitfieldSent{bitfield: bitfield},
    _metadata
  ) do
    {:ok, bitfield} = Base.decode16(bitfield)
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
    {:ok, msg} = Messages.encode(message)
    NetStats.add_sent_bytes(byte_size(msg))
    :ok
  end

  def error(error, failed_event, _context) do
    Logger.error("NetStatsUpdater failed to process event #{inspect failed_event} due to error #{inspect error}")
    :skip
  end
end
