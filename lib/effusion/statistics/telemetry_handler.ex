defmodule Effusion.Statistics.TelemetryHandler do
  alias Effusion.PWP.Messages
  alias Effusion.Statistics.Session, as: SessionStats
  alias Effusion.Statistics.Peer, as: PeerStats
  require Logger

  @moduledoc """
  Handles events emitted by the `:telemetry` library.
  """

  @doc """
  Attach all telemetry handlers.
  """
  def init do
    :ok = :telemetry.attach(
      "effusion-handler-message-sent",
      [:pwp, :message_sent],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-message-received",
      [:pwp, :message_received],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-outgoing-connection-establishing",
      [:pwp, :outgoing, :starting],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-outgoing-connection-success",
      [:pwp, :outgoing, :success],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-outgoing-connection-failure",
      [:pwp, :outgoing, :failure],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-incoming-connection-establishing",
      [:pwp, :incoming, :starting],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-incoming-connection-success",
      [:pwp, :incoming, :success],
      &__MODULE__.handle_event/4,
      nil)

    :ok = :telemetry.attach(
      "effusion-handler-disconnect",
      [:pwp, :disconnect],
      &__MODULE__.handle_event/4,
      nil)

  end

  @doc """
  Handle a telemetry event.
  """
  def handle_event(event, measurements, metadata, config)

  def handle_event([:pwp, :message_sent], _measurements, metadata, _config) do
    Logger.debug("Sending message #{inspect(metadata.message)} to peer #{metadata.peer}")

    SessionStats.inc_outgoing_message(metadata.message)
  end

  def handle_event([:pwp, :message_received], measurements, metadata, _config) do
    data = metadata.binary
    data_size = byte_size(data)
    payload_size = Messages.payload_bytes_count(data)
    msg = metadata.message


    SessionStats.inc_incoming_message(msg)
    :ets.update_counter(NetStatsTable, :recv_payload_bytes, payload_size, {:k, 0})
    :ets.update_counter(NetStatsTable, :recv_bytes, data_size, {:k, 0})

    if Map.has_key?(metadata, :info_hash) do
      :ets.update_counter(TorrentDownloadStatsTable, data_size, data_size, {:k, 0})
    end

    if Map.has_key?(metadata, :remote_peer_id) do
      Logger.debug("Receiving message from #{metadata.remote_peer_id}: #{inspect msg}. Process latency: #{measurements.latency} ms.")
      :ets.update_counter(PeerDownloadStatsTable, metadata.remote_peer_id, data_size, {:k, 0})
    end
  end

  def handle_event([:pwp, :outgoing, :starting], _measurements, metadata, _config) do
    Logger.debug("Establishing connection to #{inspect metadata.address}")
    PeerStats.inc_num_peers_half_open()
  end

  def handle_event([:pwp, :outgoing, :success], _measurements, metadata, _config) do
    Logger.debug("Handshake with #{inspect metadata.address} successful")
    PeerStats.inc_num_tcp_peers()
    PeerStats.dec_num_peers_half_open()
  end

  def handle_event([:pwp, :outgoing, :failure], _measurements, _metadata, _config) do
    PeerStats.dec_num_peers_half_open()
  end

  def handle_event([:pwp, :incoming, :starting], _measurements, metadata, _config) do
    Logger.debug("Receiving handshake from #{metadata.remote_peer_id}")
  end

  def handle_event([:pwp, :incoming, :success], _measurements, _metadata, _config) do
    PeerStats.inc_num_tcp_peers()
  end

  def handle_event([:pwp, :disconnect], _measurements, metadata, _config) do
    if metadata.reason != :handshake_failure do
      PeerStats.dec_num_tcp_peers()
    end

    cond do
      Map.has_key?(metadata, :remote_peer_id) ->
        Logger.debug("Connection handler for #{metadata.remote_peer_id} terminating with reason #{inspect(metadata.reason)}")
      Map.has_key?(metadata, :address) ->
        Logger.debug("Connection handler for #{inspect metadata.address} terminating with reason #{inspect(metadata.reason)}")
      true ->
        Logger.debug("Connection handler #{inspect self()} terminating with reason #{inspect metadata.reason}")
    end
  end
end
