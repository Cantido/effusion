defmodule Effusion.PWP.Peer do
  require Logger
  alias Effusion.LocalPeer
  @behaviour Effusion.PWP.GenPeer

  def handle_handshake({peer_id, info_hash, _reserved}, _state) do
    with :ok <- check_info_hash(info_hash),
         :ok <- check_peer_id(peer_id),
         local_peer_id <- LocalPeer.peer_id()
    do
      :ok = Logger.info ("Handshake from peer_id #{Base.encode16(peer_id)} for info_hash #{Base.encode16(info_hash)}")
      {:ok,
        {local_peer_id, info_hash, <<0 :: 64>>},
        %{remote_peer_id: peer_id}}
    else
      err -> err
    end
  end

  defp check_info_hash(info_hash) do
    case Registry.lookup(Effusion.TorrentRegistry, info_hash) do
      [{_pid, :ok}] -> :ok
      [] -> {:error, :unknown_info_hash}
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
