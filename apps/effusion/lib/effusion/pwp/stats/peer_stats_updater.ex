defmodule Effusion.PWP.EventHandlers.PeerStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.Commanded,
    name: __MODULE__

  alias Effusion.Statistics.Peer, as: PeerStats

  alias Effusion.PWP.Connection.Events.{
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
    :ok
  end

  def handle(
        %PeerDisconnected{},
        _metadata
      ) do
    PeerStats.dec_num_tcp_peers()
    :ok
  end

  def handle(
        %AttemptingToConnect{},
        _metadata
      ) do
    PeerStats.inc_num_peers_half_open()
    :ok
  end

  def handle(
        %PeerConnected{initiated_by: "us"},
        _metadata
      ) do
    PeerStats.dec_num_peers_half_open()
    :ok
  end

  def handle(
        %ConnectionAttemptFailed{},
        _metadata
      ) do
    PeerStats.dec_num_peers_half_open()
    :ok
  end

  def error(error, failed_event, _context) do
    Logger.error(
      "PeerStatsUpdater failed to process event #{inspect(failed_event)} due to error #{
        inspect(error)
      }"
    )

    :skip
  end
end
