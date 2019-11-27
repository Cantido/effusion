defmodule Effusion.PWP.ConnectionRegistry do
  alias Effusion.PWP.OutgoingHandler
  import Effusion.Hash, only: [is_hash: 1]
  import Effusion.BTP.Peer
  require Logger

  @moduledoc """
  A registry of connected peers and the processes handling them.
  """

  def register(info_hash, peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    _ =
      case connections do
        [{_conn_pid, ^peer_id}] -> {:error, :already_connected}
        [] -> Registry.register(ConnectionRegistry, info_hash, peer_id)
      end

  end

  def connected?(info_hash, peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    case connections do
      [{_conn_pid, ^peer_id}] -> true
      [] -> false
    end
  end

  def disconnect_all(info_hash) do
    Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
      connections
      |> Enum.map(fn {c, _p} -> OutgoingHandler.disconnect(c) end)
    end)
  end

  def btp_broadcast(info_hash, message, peer_id_selector \\ fn _ -> true end) do
    :ok =
      Registry.dispatch(ConnectionRegistry, info_hash, fn connections ->
        connections
        |> Enum.filter(fn {_, peer_id} -> peer_id_selector.(peer_id) end)
        |> Enum.each(fn {c, id} -> send(c, {:btp_send, id, message}) end)
      end)
  end

  def btp_send(info_hash, peer_id, message) when is_hash(info_hash) and is_peer_id(peer_id) do
    connections = Registry.match(ConnectionRegistry, info_hash, peer_id)

    _ =
      case connections do
        [{conn_pid, ^peer_id}] -> send(conn_pid, {:btp_send, peer_id, message})
        [] -> []
      end
  end
end
