defmodule Effusion.BTP.SessionServer do
  use GenServer
  require Logger
  alias Effusion.BTP.Session

  @moduledoc """
  An API to manage a `Effusion.BTP.Session` object as it is connected to many peers simultaneously.
  """

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  @doc """
  Start the Session Server in its own supervision tree.
  """
  def start(meta, {_host, _port} = local_server, file) when is_map(meta) do
    Effusion.Application.SessionServerSupervisor.start_child([meta, local_server, file])
  end

  @doc """
  Start the session server and link it to the current process.
  """
  def start_link([meta, local_peer, file]) do
    GenServer.start_link(__MODULE__, [meta, local_peer, file])
  end

  @doc """
  Wait on a download managed by a session server to complete.
  """
  def await(pid) do
    GenServer.call(pid, :await, :infinity)
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(pid, peer_id, message) do
    GenServer.call(pid, {:handle_msg, peer_id, message})
  end

  @doc """
  Handle a peer disconnection.
  """
  def unregister_connection(pid, peer_id, address \\ nil) do
    GenServer.call(pid, {:unregister_connection, peer_id, address})
  end

  ## Callbacks

  def init([meta, local_peer, file]) do
    state = Session.new(meta, local_peer, file)

    {:ok, state, 0}
  end

  def handle_call({:handle_msg, peer_id, msg}, _from, state) do
    {state, messages} = Session.handle_message(state, peer_id, msg)
    Logger.debug("replying: #{inspect(messages)}")

    [{conn_pid, ^peer_id}] = Registry.match(ConnectionRegistry, state.meta.info_hash, peer_id)

    Enum.map(messages, fn(m) ->
      send(conn_pid, {:btp_send, m})
    end)

    {:reply, :ok, state}
  end

  def handle_call(:await, {from_pid, _from_ref}, state) do
    state = Session.add_listener(state, from_pid)
    {:noreply, state}
  end

  def handle_call({:unregister_connection, peer_id, address}, _from, state) do
    {:reply, :ok, Session.handle_disconnect(state, peer_id, address)}
  end

  def handle_info(:timeout, state) do
    {:noreply, Session.start(state, @thp_client)}
  end

  def handle_info(:interval_expired, state) do
    {:noreply, Session.announce(state, @thp_client)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(:normal, state) do
    Session.announce(state, @thp_client, :stopped)
    reply_to_listeners(state, {:ok, Session.torrent(state)})
  end

  def terminate(reason, state) do
    Session.announce(state, @thp_client, :stopped)
    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state, msg) do
    Session.each_listener(state, fn l -> GenServer.reply(l, msg) end)
  end
end
