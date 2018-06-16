defmodule Effusion.BTP.SessionServer do
  use GenServer
  require Logger
  alias Effusion.BTP.Session

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  def start(meta, {_host, _port} = local_server, file) when is_map(meta) do
    Effusion.Application.SessionServerSupervisor.start_child([meta, local_server, file])
  end

  def start_link([meta, local_peer, file]) do
    GenServer.start_link(__MODULE__, [meta, local_peer, file])
  end

  def await(pid) do
    GenServer.call(pid, :await, :infinity)
  end

  def handle_message(pid, peer_id, message) do
    GenServer.call(pid, {:handle_msg, peer_id, message})
  end

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
    {:reply, messages, state}
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
    reply_to_listeners(state, {:ok, Session.torrent(state)})
  end

  def terminate(reason, state) do
    reply_to_listeners(state, {:error, :torrent_crashed, [reason: reason]})
  end

  defp reply_to_listeners(state, msg) do
    Session.each_listener(state, fn l -> GenServer.reply(l, msg) end)
  end
end
