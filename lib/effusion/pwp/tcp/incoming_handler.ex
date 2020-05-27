defmodule Effusion.PWP.TCP.IncomingHandler do
  alias Effusion.PWP.TCP.Connection
  require Logger

  @behaviour :ranch_protocol

  @moduledoc """
  Ranch protocol handler for incoming PWP connections.
  """

  @impl true
  def start_link(ref, socket, transport, _opts) do
    pid = :proc_lib.spawn_link(Connection, :incoming_init, [ref, socket, transport])
    {:ok, pid}
  end
end
