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
    {:ok, %{parent_process: parent_process, lsock: lsock}, 0}
  end

  def handle_info(:timeout, %{lsock: lsock} = state) do
    {:ok, sock} = :gen_tcp.accept(lsock)
    :ok = :inet.setopts(sock, active: true, packet: 0, packet_size: 68)
    {:noreply, Map.put(state, :sock, sock)}
  end

  def handle_info({:tcp, _sock, packet}, state) do
    packet |> IO.iodata_to_binary() |> handle_packet(state)
  end

  ## TCP-specific callbacks for handling any binary packet

  def handle_packet(packet, %{parent_process: parent_process} = state) when byte_size(packet) == 68 do
    {:ok, hs} =  Handshake.decode(packet)
    send parent_process, {:handshake, hs}
    {:noreply, state}
  end

  def handle_packet(packet, %{parent_process: parent_process} = state) do
    {:ok, msg} = Messages.decode(packet)
    send parent_process, msg
    {:noreply, state}
  end
end
