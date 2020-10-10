defmodule Effusion.CQRS.EventHandlers.PeerStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.CQRS.Events.{
    PeerConnected,
    PeerDisconnected,
    AttemptingToConnect,
    ConnectionAttemptFailed
  }
  require Logger

  def handle(
    %PeerConnected{},
    _metadata
  ) do
    PeerStats.inc_num_tcp_peers()
  end

  def handle(
    %PeerDisconnected{},
    _metadata
  ) do
    PeerStats.dec_num_tcp_peers()
  end

  def handle(
    %AttemptingToConnect{},
    _metadata
  ) do
    PeerStats.dec_num_peers_half_open()
  end

  def handle(
    %ConnectionAttemptFailed{},
    _metadata
  ) do
    PeerStats.dec_num_peers_half_open()
  end

  def error(error, failed_event, _context) do
    Logger.error("PeerStatsUpdater failed to process event #{inspect failed_event} due to error #{inspect error}")
    :skip
  end
end
