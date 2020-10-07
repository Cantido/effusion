defmodule Effusion.PWP.ProtocolHandler do
  alias Effusion.BTP.Block
  alias Effusion.BTP.Peer
  alias Effusion.BTP.PeerPiece
  alias Effusion.BTP.Piece
  alias Effusion.BTP.Pieces
  alias Effusion.BTP.Request
  alias Effusion.BTP.Torrent
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.PWP.TCP.Connection
  alias Effusion.Repo
  import Effusion.BTP.Peer
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  Handles Peer Wire Protocol messages.
  """

  defp local_peer_id do
    Application.get_env(:effusion, :peer_id)
  end

  defp supported_extensions do
    Application.fetch_env!(:effusion, :enabled_extensions)
  end

  @doc """
  Connect to the remote address, expecting the given peer ID.
  """
  def connect(address, info_hash, remote_peer_id) do
    # This is where we would make the uTP/TCP decision, once we support uTP.
    Connection.connect({address, info_hash, remote_peer_id})
  end

  @doc """
  Break the connection with the given peer.
  """
  def disconnect(info_hash, remote_peer_id, reason) do
    Connection.disconnect(info_hash, remote_peer_id, reason)
  end

  @doc """
  Get the handshake tuple for this connection.
  """
  def get_handshake(info_hash) do
    {:handshake, local_peer_id(), info_hash, supported_extensions()}
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
    :ok
  end

  def next_request_from_peer(info_hash, peer_id, count) when is_hash(info_hash) do
    requests = Request.valid_requests_from_peer_query(info_hash, peer_id, count)
    |> Repo.all()

    requests_to_insert = Enum.map(requests, fn {_piece, block, peer} ->
      %{
        block_id: block.id,
        peer_id: peer.id
      }
    end)
    Repo.insert_all(Request, requests_to_insert)

    Enum.each(requests, fn {piece, block, peer} ->
      ConnectionRegistry.btp_send(
        info_hash,
        peer.peer_id,
        {:request, piece.index, block.offset, block.size}
      )
    end)
    :ok
  end

  @doc """
  Disconnect from all peers.
  """
  def disconnect_all(info_hash) do
    ConnectionRegistry.disconnect_all(info_hash)
  end
end
