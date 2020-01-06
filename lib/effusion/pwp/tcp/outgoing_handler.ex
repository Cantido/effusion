defmodule Effusion.PWP.TCP.OutgoingHandler do
  use GenServer, restart: :temporary
  alias Effusion.Application.ConnectionSupervisor
  alias Effusion.PWP.TCP.Connection
  alias Effusion.PWP.ConnectionRegistry
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

  @doc """
  Start a connection to a `peer`, and link the resulting process to the current process.
  """
  def start_link(peer = {{_host, port}, info_hash, expected_peer_id}) when is_integer(port) and is_hash(info_hash) and is_peer_id(expected_peer_id)  do
    GenServer.start_link(__MODULE__, peer)
  end

  def disconnect(pid) do
    Connection.disconnect(pid)
  end

  def disconnect(info_hash, peer_id, reason) do
    pid = ConnectionRegistry.get_pid(info_hash, peer_id)
    Connection.disconnect(pid, reason)
  end

  ## Callbacks

  @impl true
  def init(peer) do
    {:ok, peer, 0}
  end

  @impl true
  def handle_info(info, state) do
    Connection.handle_info(info, state)
  end

  @impl true
  def terminate(reason, state) do
    Connection.terminate(reason, state)
  end
end
