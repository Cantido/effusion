defmodule Effusion.PWP.PeerSocket do
  use GenServer
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Messages.Handshake

  ## API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  ## Callbacks

  def init([parent_process, lsock]) do
    {:ok, %{parent_process: parent_process, lsock: lsock, handshaken: false}, 0}
  end

  def handle_info(:timeout, %{lsock: lsock} = state) do
    {:ok, sock} = :gen_tcp.accept(lsock)
    :ok = :inet.setopts(sock, [:binary, active: true, packet: 0])
    {:noreply, Map.put(state, :sock, sock)}
  end

  def handle_info({:tcp, sock, packet}, %{parent_process: parent_process, handshaken: false} = state) when byte_size(packet) == 68 do
    {:ok, hs} = Handshake.decode(packet)
    send parent_process, {:handshake, hs}

    :ok = :inet.setopts(sock, active: true, packet: 4)
    {:noreply, %{state | handshaken: true}}
  end

  def handle_info({:tcp, _sock, packet}, %{parent_process: parent_process, handshaken: true} = state) do
    {:ok, msg} = Messages.decode(packet)
    send parent_process, msg
    {:noreply, state}
  end

  def handle_info({:tcp, _sock, _packet}, %{handshaken: false} = state) do
    {:stop, :unexpected_message_before_handshake, state}
  end

  def handle_info({:tcp, _, _}, state) do
    {:stop, :bad_message, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
