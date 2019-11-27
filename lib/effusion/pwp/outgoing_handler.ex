defmodule Effusion.PWP.OutgoingHandler do
  use GenServer, restart: :temporary
  alias Effusion.Application.ConnectionSupervisor
  alias Effusion.PWP.Connection
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
  def connect(peer)do
    ConnectionSupervisor.start_child(peer)
  end

  @doc """
  Start a connection to a `peer`, and link the resulting process to the current process.
  """
  def start_link(peer) do
    GenServer.start_link(__MODULE__, peer)
  end

  def disconnect(pid) do
    Connection.disconnect(pid)
  end

  ## Callbacks

  def init(peer) do
    {:ok, peer, 0}
  end

  def handle_info(info, state) do
    Connection.handle_info(info, state)
  end

  def terminate(reason, state) do
    Connection.terminate(reason, state)
  end
end
