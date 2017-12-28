require Logger
alias Effusion.Messages.Handshake
alias Effusion.{LocalPeer, Torrents}

defmodule Effusion.PeerConnection do
  @moduledoc """
  Maintain a TCP connection with a peer.
  """

  @doc """
  Wait for a handshake on the given socket, and respond when you get one.
  On any error, or when the handshake is completed, the socket is shut down.
  """
  def serve(socket) do
    :ok = handle_handshake(socket)
    :ok = handle_messages(socket)
    :gen_tcp.shutdown(socket, :read_write)
  end

  defp handle_handshake(socket) do
    with {:ok, data} <- :gen_tcp.recv(socket, 0),
         {:ok, peer_id, info_hash, _reserved} <- Handshake.decode(data),
         :ok = Torrents.lookup(info_hash),
         :ok = check_peer_id(LocalPeer.peer_id(), peer_id)
    do
      :ok = Logger.info ("Handshake from peer_id #{Base.encode16(peer_id)} for info_hash #{Base.encode16(info_hash)}")
      response = Handshake.encode( LocalPeer.peer_id(), info_hash)
      :ok = :gen_tcp.send(socket, response)
      :ok
    else
      err -> err
    end
  end

  defp check_peer_id(local_peer_id, remote_peer_id) do
    if(local_peer_id == remote_peer_id) do
      {:error, :remote_same_as_local}
    else
      :ok
    end
  end

  defp handle_messages(_socket) do
    :ok
  end
end
