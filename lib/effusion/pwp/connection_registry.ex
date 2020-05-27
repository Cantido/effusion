defmodule Effusion.PWP.ConnectionRegistry do
  alias Effusion.PWP.TCP.Connection
  import Effusion.Hash, only: [is_hash: 1]
  import Effusion.BTP.Peer
  require Logger

  @moduledoc """
  A registry of connected peers and the processes handling them.
  """

  @doc """
  Register the current process as a connection process with the given peer.
  """
  def register(info_hash, peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    _ =
      case connections do
        [{_conn_pid, ^peer_id}] -> {:error, :already_connected}
        [] -> Registry.register(ConnectionRegistry, info_hash, peer_id)
      end

  end

  @doc """
  Get the process ID of the connection with the given peer.
  """
  def get_pid(info_hash, peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    case connections do
      [{conn_pid, ^peer_id}] -> conn_pid
      _ -> nil
    end
  end

  @doc """
  Check if a peer is connected.
  """
  def connected?(info_hash, peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    case connections do
      [{_conn_pid, ^peer_id}] -> true
      [] -> false
    end
  end

  @doc """
  Get all connected processes for the given torrent.
  """
  def all_connected(info_hash) do
    Registry.match(ConnectionRegistry, info_hash, :_)
    |> Enum.map(fn {_conn_pid, peer_id} ->
      peer_id
    end)
  end

  @doc """
  Break all connections for the given torrent.
  """
  def disconnect_all(info_hash) do
    Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
      connections
      |> Enum.map(fn {c, _p} -> Connection.disconnect(c) end)
    end)
  end

  @doc """
  Broadcast a Peer Wire Protocol message to all connected peers.
  """
  def btp_broadcast(info_hash, message, peer_id_selector \\ fn _ -> true end) do
    :ok =
      Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
        connections
        |> Enum.filter(fn {_, peer_id} -> peer_id_selector.(peer_id) end)
        |> Enum.each(fn {c, id} -> send(c, {:btp_send, id, message}) end)
      end)
  end

  @doc """
  Send a Peer Wire Protocol message to the given peer.
  """
  def btp_send(info_hash, peer_id, message) when is_hash(info_hash) and is_peer_id(peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    _ =
      case connections do
        [{conn_pid, ^peer_id}] -> send(conn_pid, {:btp_send, peer_id, message})
        [] -> []
      end
  end
end
