defmodule Effusion.Application.SessionServer do
  use GenServer
  require Logger
  alias Effusion.BTP.Session
  alias Effusion.Hash
  alias Effusion.PWP.Socket

  @thp_client Application.get_env(:effusion, :thp_client)

  ## API

  def start(meta, {_host, _port} = local_server, file \\ nil) when is_map(meta) do
    Effusion.Application.SessionServerSupervisor.start_child([meta, local_server, file])
  end

  def start_link([meta, local_peer]) do
    start_link([meta, local_peer, nil])
  end

  def start_link([meta, local_peer, file]) do
    GenServer.start_link(__MODULE__, [meta, local_peer, file])
  end

  def block(pid, block) do
    GenServer.call(pid, {:block, block})
  end

  def blocks(pid) do
    GenServer.call(pid, :get_blocks)
  end

  def next_request(pid) do
    GenServer.call(pid, :next_request)
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

  def handle_message(pid, peer_id, msg) do
      GenServer.call(pid, {:handle_msg, peer_id, msg})
  end

  def handle_packet(pid, peer_id, data, socket) do
    with {:ok, msg1} <- Socket.decode(data),
         :ok <- Logger.info "Got message #{inspect(msg1)}"
    do
      messages = handle_message(pid, peer_id, msg1)
      Enum.map(messages, fn(m) -> :ok = Socket.send_msg(socket, m) end)
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  ## Callbacks

  defguard is_index(i) when is_integer(i) and i >= 0
  defguard is_size(x) when is_integer (x) and x > 0

  def init([meta, local_peer, file]) do
    state = Session.new(meta, local_peer, file)

    {:ok, state, 0}
  end

  def handle_call({:handle_msg, peer_id, msg}, _from, state) do
    {state, messages} = Session.handle_msg(state, peer_id, msg)
    {:reply, messages, state}
  end

  def handle_call(:get_blocks, _from, state) do
    {:reply, Session.blocks(state), state}
  end

  def handle_call({:block, block}, _from, state) do
    state = state
    |> Session.add_block(block)
    |> Session.write()

    if(Session.done?(state)) do
      {:stop, :normal, :ok, state}
    else
      {:reply, :ok, state}
    end

  end

  def handle_call(:next_request, _from, state) do
    {next_block, state1} = Session.next_request(state)

    {:reply, next_block, state1}
  end

  def handle_call(:await, from, state) do
    state = Session.add_listener(state, from)
    {:noreply, state}
  end

  def handle_info(:timeout, state) do
    _ = Logger.info("Announcing torrent #{Hash.inspect state.meta.info_hash} to #{inspect(state.meta.announce)} that I'm listening at #{inspect(state.local_peer)}")

    state = Session.announce(state, @thp_client)

    _ = Logger.info("Announce finished, got #{Enum.count(state.peers)} peers.")

    state = Session.increment_connections(state)

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(:normal, state) do
    Session.write(state)

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