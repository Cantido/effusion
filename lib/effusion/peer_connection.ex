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
    with {:ok, data} <- :gen_tcp.recv(socket, 0),
         {:ok, peer_id, info_hash, _reserved} = Handshake.decode(data),
         :ok = Torrents.lookup(info_hash)
    do
      Logger.info ("Handshake from peer_id #{inspect(peer_id)} for info_hash #{inspect(info_hash)}")
      response = Handshake.encode(info_hash, LocalPeer.peer_id())
      :gen_tcp.send(socket, response)
      :gen_tcp.shutdown(socket, :read_write)
    else
      _err -> :gen_tcp.shutdown(socket, :read_write)
    end
  end

end
