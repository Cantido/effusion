require Logger
alias Effusion.PWP.Messages.Handshake
alias Effusion.PWP.Peer

defmodule Effusion.PWP.Connection do
  use GenServer, restart: :temporary
  @moduledoc """
  Maintain a TCP connection with a peer.
  """

  ## API

  def start_link(_, socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  ## Callbacks

  @pre_handshake_socket_options [packet: 0,
                                 packet_size: 68,
                                 active: true]

  @post_handshake_socket_options [packet: 4,
                                  packet_size: 0,
                                  active: true]

  def init(socket) do
    :inet.setopts(socket, @pre_handshake_socket_options)
    {:ok, %{socket: socket, peer_state: %{}}}
  end

  def handle_info({:tcp, socket, <<data::bytes-size(68)>>}, state) do
    with {:ok, peer_id, info_hash, reserved} <- Handshake.decode(data),
         {:ok, {local_peer_id, local_hash, _local_reserved}, peer_state} <- Peer.handle_handshake({peer_id, info_hash, reserved}, state.peer_state)
    do
      response = Handshake.encode(local_peer_id, local_hash)
      :ok = :gen_tcp.send(socket, response)
      {:noreply, %{state | peer_state: peer_state}}
    else
      err -> {:stop, err, state}
    end
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end
end
