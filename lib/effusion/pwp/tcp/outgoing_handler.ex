defmodule Effusion.PWP.TCP.OutgoingHandler do
  alias Effusion.Application.ConnectionSupervisor
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.Connection
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Effusion.Hash, only: [is_hash: 1]
  require Logger

  @moduledoc """
  A connection to a peer.

  Allows for network IO to happen in a separate process,
  and sends PWP messages back to the parent session server.
  """

  ## API

  @doc """
  Start a connection to a `peer` in the Connection supervision hierarchy.
  """
  def connect(peer = {{_host, port}, info_hash, expected_peer_id}) when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id) do
    ConnectionSupervisor.start_child(peer)
  end

  def disconnect(pid) do
    Connection.disconnect(pid)
  end

  def disconnect(info_hash, peer_id, reason) do
    pid = ConnectionRegistry.get_pid(info_hash, peer_id)
    Connection.disconnect(pid, reason)
  end
end
