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
end
