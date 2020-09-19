defmodule Effusion.Statistics.TelemetryHandler do
  alias Effusion.PWP.Messages
  alias Effusion.Statistics.Peer, as: PeerStats
  alias Effusion.Statistics.Session, as: SessionStats
  alias Effusion.Statistics.Net, as: NetStats
  require Logger

  @moduledoc """
  Handles events emitted by the `:telemetry` library.
  """

  @handlers %{
    "effusion-handler-message-sent" => [:pwp, :message_sent],
    "effusion-handler-message-received" => [:pwp, :message_received],
    "effusion-handler-outgoing-connection-establishing" => [:pwp, :outgoing, :starting],
    "effusion-handler-outgoing-connection-success" => [:pwp, :outgoing, :success],
    "effusion-handler-outgoing-connection-failure" => [:pwp, :outgoing, :failure],
    "effusion-handler-incoming-connection-establishing" => [:pwp, :incoming, :starting],
    "effusion-handler-incoming-connection-success" => [:pwp, :incoming, :success],
    "effusion-handler-disconnect" => [:pwp, :disconnect],
    "effusion-handler-write-piece-start" => [:io, :write, :piece, :starting],
    "effusion-handler-write-piece-success" => [:io, :write, :piece, :success],
    "effusion-handler-torrent-started" => [:btp, :started],
    "effusion-handler-torrent-completed" => [:btp, :completed]
  }

  @doc """
  Attach all telemetry handlers.
  """
  def init do
    Enum.each(@handlers, fn {name, event} ->
      :ok = :telemetry.attach(name, event, &__MODULE__.handle_event/4, nil)
    end)
  end

  @doc """
  Handle a telemetry event.
  """
  def handle_event(event, measurements, metadata, config)

  def handle_event([:pwp, :message_sent], _measurements, metadata, _config) do
    SessionStats.inc_outgoing_message(metadata.message)
  end

  def handle_event([:pwp, :message_received], _measurements, metadata, _config) do
    data = metadata.binary
    data_size = byte_size(data)
    payload_size = Messages.payload_bytes_count(data)
    msg = metadata.message

    SessionStats.inc_incoming_message(msg)
    NetStats.add_recv_bytes(data_size)
    NetStats.add_recv_payload_bytes(payload_size)

    if Map.has_key?(metadata, :info_hash) do
      NetStats.add_torrent_recv_payload_bytes(metadata.info_hash, payload_size)
    end

    if Map.has_key?(metadata, :remote_peer_id) do
      NetStats.add_peer_recv_bytes(metadata.remote_peer_id, data_size)
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

  def handle_event([:io, :write, :piece, :starting], _measurements, %{index: index, info_hash: info_hash}, _config) do
    Logger.debug("Writing piece #{index} for #{info_hash |> Effusion.Hash.encode()}...")
  end

  def handle_event([:io, :write, :piece, :success], %{latency: latency}, %{index: index, info_hash: info_hash}, _config) do
    Logger.debug("Done writing piece #{index} for #{info_hash |> Effusion.Hash.encode()}. Took #{latency} us.")
  end

  def handle_event([:btp, :started], %{}, %{info_hash: info_hash}, _config) do
    Logger.info("Download #{info_hash |> Effusion.Hash.encode()} started.")
  end

  def handle_event([:btp, :completed], %{}, %{info_hash: info_hash}, _config) do
    Logger.info("Download #{info_hash |> Effusion.Hash.encode()} finished.")
  end
end
