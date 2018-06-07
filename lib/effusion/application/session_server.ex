defmodule Effusion.Application.SessionServer do
  use GenServer
  require Logger
  alias Effusion.BTP.Session
  alias Effusion.PWP.Socket

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

  def connect(_, peer) do
    with {:ok, socket, peer} <- Socket.connect(peer),
         peer <- Map.put(peer, :socket, socket)
    do
      {:ok, peer}
    else
      _ -> {:error, :failed_handshake}
    end
  end

  def handle_packet(pid, peer_id, data, socket) do
    with {:ok, msg1} <- Socket.decode(data),
         :ok <- Logger.info "Got message #{inspect(msg1)}"
    do
      messages = GenServer.call(pid, {:handle_msg, peer_id, msg1})
      Enum.map(messages, fn(m) -> :ok = Socket.send_msg(socket, m) end)
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  ## Callbacks

  def init([meta, local_peer, file]) do
    state = Session.new(meta, local_peer, file)

    {:ok, state, 0}
  end

  def handle_call({:handle_msg, peer_id, msg}, _from, state) do
    {state, messages} = Session.handle_msg(state, peer_id, msg)
    {:reply, messages, state}
  end

  def handle_call(:await, from, state) do
    state = Session.add_listener(state, from)
    {:noreply, state}
  end

  def handle_info(:timeout, state) do
    {:noreply, Session.start(state, @thp_client)}
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
    Enum.each(
      Session.listeners(state),
      fn l -> GenServer.reply(l, msg) end
    )
  end
end
