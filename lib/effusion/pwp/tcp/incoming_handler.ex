defmodule Effusion.PWP.TCP.IncomingHandler do
  alias Effusion.PWP.TCP.Connection
  use GenServer, restart: :temporary
  require Logger

  @behaviour :ranch_protocol

  @moduledoc """
  Ranch protocol handler for incoming PWP connections.
  """

  @impl true
  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(__MODULE__, :init, [ref, socket, transport])
    {:ok, pid}
  end

  @impl true
  def init(init_arg) do
    {:ok, init_arg}
  end

  def init(ref, socket, transport) do
    _ = Logger.debug("Starting protocol")

    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: :once)
    :gen_server.enter_loop(__MODULE__, [], %{socket: socket, transport: transport})
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
