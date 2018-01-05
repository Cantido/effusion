require Logger
alias Effusion.PWP.Messages.Handshake
alias Effusion.LocalPeer

defmodule Effusion.PWP.PeerConnection do
  use GenServer, restart: :temporary
  @moduledoc """
  Maintain a TCP connection with a peer.
  """

  ## API

  def start_link(_, socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  ## Callbacks

  def init(socket) do
    {:ok, socket}
  end

  def handle_info({:tcp, socket, <<data::bytes-size(68)>>}, state) do
    with {:ok, peer_id, info_hash, _reserved} <- Handshake.decode(data),
         [{_pid, :ok}] = Registry.lookup(Effusion.TorrentRegistry, info_hash),
         :ok = check_peer_id(peer_id)
    do
      :ok = Logger.info ("Handshake from peer_id #{Base.encode16(peer_id)} for info_hash #{Base.encode16(info_hash)}")
      response = Handshake.encode(LocalPeer.peer_id(), info_hash)
      :ok = :gen_tcp.send(socket, response)
      {:noreply}
    else
      err -> {:stop, err, state}
    end
  end

  @spec check_peer_id(Effusion.PeerId.t) :: :ok | {:error, :remote_same_as_local}
  defp check_peer_id(remote_peer_id) do
    if LocalPeer.matches_id?(remote_peer_id) do
      {:error, :remote_same_as_local}
    else
      :ok
    end
  end
end
