defmodule Effusion.PWP.ProtocolHandler do
  alias Effusion.PWP.TCP.Connection
  require Logger

  @moduledoc """
  Handles Peer Wire Protocol messages.
  """

  defp local_peer_id do
    Application.get_env(:effusion, :peer_id)
  end

  defp supported_extensions do
    Application.fetch_env!(:effusion, :enabled_extensions)
  end

  @doc """
  Connect to the remote address, expecting the given peer ID.
  """
  def connect(address, info_hash, remote_peer_id) do
    # This is where we would make the uTP/TCP decision, once we support uTP.
    Connection.connect({address, info_hash, remote_peer_id})
  end

  @doc """
  Get the handshake tuple for this connection.
  """
  def get_handshake(info_hash) do
    {:handshake, local_peer_id(), info_hash, supported_extensions()}
  end

  @doc """
  Validate and handle the given handshake tuple.
  """
  def recv_handshake({:handshake, _remote_peer_id, info_hash, _extensions}) do
    case Registry.lookup(BTPHandlerRegistry, info_hash) do
      [{_pid, _hash}] -> :ok
      _ -> {:error, :torrent_not_found}
    end
  end

  @doc """
  Validate and handle the given handshake tuple, expecting a given info hash and peer ID.
  """
  def recv_handshake({:handshake, remote_peer_id, remote_info_hash, _extensions}, info_hash, expected_peer_id) do
    with :ok <- validate_info_hash(info_hash, remote_info_hash),
         :ok <- validate_peer_id(expected_peer_id, remote_peer_id) do
      :ok
    else
      err -> err
    end
  end

  defp validate_info_hash(local_info_hash, remote_info_hash) do
    if local_info_hash == remote_info_hash do
      :ok
    else
      {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}
    end
  end

  defp validate_peer_id(expected_peer_id, remote_peer_id) do
    if expected_peer_id == nil or expected_peer_id == remote_peer_id do
      :ok
    else
      {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}
    end
  end
end
