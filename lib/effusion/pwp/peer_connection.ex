require Logger
alias Effusion.PWP.Messages.Handshake
alias Effusion.LocalPeer

defmodule Effusion.PWP.PeerConnection do
  @moduledoc """
  Maintain a TCP connection with a peer.
  """

  @typedoc """
    Reasons that a handshake would fail.
  """
  @type handshake_failure_reason ::
      :remote_same_as_local
    | :closed
    | Handshake.decode_failure_reason
    | :inet.posix()

  @doc """
  Wait for a handshake on the given socket, and respond when you get one.
  On any error, or when the handshake is completed, the socket is shut down.
  """
  @spec serve(:gen_tcp.socket()) :: :ok | {:error, handshake_failure_reason}
  def serve(socket) do
    {:ok, data} = :gen_tcp.recv(socket, 68)
    {:noreply} = handle_info({:tcp, socket, data}, {})
  end

  def handle_info({:tcp, socket, <<data::bytes-size(68)>>}, state) do
    with {:ok, peer_id, info_hash, _reserved} <- Handshake.decode(data),
         [{_pid, :ok}] = Registry.lookup(Effusion.TorrentRegistry, info_hash),
         :ok = check_peer_id(LocalPeer.peer_id(), peer_id)
    do
      :ok = Logger.info ("Handshake from peer_id #{Base.encode16(peer_id)} for info_hash #{Base.encode16(info_hash)}")
      response = Handshake.encode(LocalPeer.peer_id(), info_hash)
      :ok = :gen_tcp.send(socket, response)
      {:noreply}
    else
      err -> {:stop, err, state}
    end
  end

  @spec check_peer_id(Effusion.PeerId.t, Effusion.PeerId.t) :: :ok | {:error, :remote_same_as_local}
  defp check_peer_id(local_peer_id, remote_peer_id) do
    if(local_peer_id == remote_peer_id) do
      {:error, :remote_same_as_local}
    else
      :ok
    end
  end
end
