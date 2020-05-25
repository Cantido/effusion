defmodule Effusion.PWP.ProtocolHandler do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.OutgoingHandler
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  Handles Peer Wire Protocol messages.
  """

  @local_peer_id Application.get_env(:effusion, :peer_id)
  @supported_extensions [:fast]

  @doc """
  Connect to the remote address, expecting the given peer ID.
  """
  def connect(address, info_hash, remote_peer_id) do
    # This is where we would make the uTP/TCP decision, once we support uTP.
    OutgoingHandler.connect({address, info_hash, remote_peer_id})
  end

  @doc """
  Break the connection with the given peer.
  """
  def disconnect(info_hash, remote_peer_id, reason) do
    OutgoingHandler.disconnect(info_hash, remote_peer_id, reason)
  end

  @doc """
  Get the handshake tuple for this connection.
  """
  def get_handshake(info_hash) do
    {:handshake, @local_peer_id, info_hash, @supported_extensions}
  end

  @doc """
  Validate and handle the given handshake tuple.
  """
  def recv_handshake({:handshake, _remote_peer_id, info_hash, _extensions}) do
    case Registry.lookup(BTPHandlerRegistry, info_hash) do
      [{_pid, _hash}] -> :ok
      _ -> {:error, :torrent_not_found}
    end
  end

  @doc """
  Validate and handle the given handshake tuple, expecting a given info hash and peer ID.
  """
  def recv_handshake({:handshake, remote_peer_id, remote_info_hash, _extensions}, info_hash, expected_peer_id) do
    with :ok <- validate_info_hash(info_hash, remote_info_hash),
         :ok <- validate_peer_id(expected_peer_id, remote_peer_id) do
      :ok
    else
      err -> err
    end
  end

  defp validate_info_hash(local_info_hash, remote_info_hash) do
    if local_info_hash == remote_info_hash do
      :ok
    else
      {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}
    end
  end

  defp validate_peer_id(expected_peer_id, remote_peer_id) do
    if expected_peer_id == nil or expected_peer_id == remote_peer_id do
      :ok
    else
      {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}
    end
  end

  @doc """
  Handle a successful connection.
  """
  def handle_connect(info_hash, peer_id, {ip, port}, extensions) when is_hash(info_hash) and is_peer_id(peer_id) do
    {:ok, _pid} = ConnectionRegistry.register(info_hash, peer_id)
    fast_extension = Enum.member?(extensions, :fast)
    case Repo.one(Peer.get(info_hash, {ip, port})) do
      nil ->
        torrent = Torrent.by_info_hash!(info_hash)
        %Peer{}
        |> Peer.changeset(%{
          torrent_id: torrent.id,
          peer_id: peer_id,
          address: %Postgrex.INET{address: ip},
          port: port,
          connected: true,
          failcount: -1,
          fast_extension: fast_extension
        })
        |> Repo.insert()
      _ ->
        Peer.get(info_hash, {ip, port})
        |> Repo.update_all(set: [connected: true], inc: [failcount: -1])
    end
    :ok
  end

  @doc """
  Handle a Peer Wire Protocol (PWP) message sent by a remote peer.
  """
  def handle_message(info_hash, from, message) when is_hash(info_hash) and is_peer_id(from) do
    Effusion.BlockingQueue.push(MessageQueue, {info_hash, from, message})
    :ok
  end

  @doc """
  Disconnect from all peers.
  """
  def disconnect_all(info_hash) do
    ConnectionRegistry.disconnect_all(info_hash)
  end

  @doc """
  Handle a peer disconnection.
  """
  def handle_disconnect(info_hash, {ip, port}, reason) do
    peer_query =
      from peer in Peer,
      join: torrent in assoc(peer, :torrent),
      where: torrent.info_hash == ^info_hash,
      where: peer.address == ^%Postgrex.INET{address: ip},
      where: peer.port == ^port

    if reason == :normal do
      Repo.update_all(peer_query, [set: [connected: false]])
    else
      Repo.update_all(peer_query, [inc: [failcount: 1], set: [connected: false]])
    end
    :ok
  end
end
