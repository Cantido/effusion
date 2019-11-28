defmodule Effusion.BTP.Swarm do
  alias Effusion.BTP.Peer
  alias Effusion.BTP.Request
  alias Effusion.Repo
  import Effusion.BTP.Peer, only: [is_peer_id: 1]
  import Ecto.Query
  require Logger

  @moduledoc """
  A collection of peers.

  Functions that manipulate or query the peers as a whole are located here.
  """

  defstruct [
    :peer_id,
    :info_hash,
    :peers,
    :peer_addresses,
    :requested_block_peers
  ]

  def new(peer_id, info_hash) do
    %__MODULE__{
      peer_id: peer_id,
      info_hash: info_hash,
      # {peer_address, peer}
      peers: Map.new(),
      # {peer_address, peer_id}
      peer_addresses: Map.new(),
      # {block_id => [peer_id]}
      requested_block_peers: Map.new()
    }
  end

  def select_peer(_swarm, peer_id) when is_peer_id(peer_id) do
    Repo.one!(from peer in Peer,
              where: peer.peer_id == ^peer_id)
  end

  def put_peer(swarm, peer) do
    Repo.insert(%Peer{
      address: %Postgrex.INET{address: peer.ip},
      port: peer.port,
      peer_id: Map.get(peer, :id, nil)
    },
    on_conflict: {:replace, [:address]},
    conflict_target: [:peer_id])
    swarm
  end

  def peers(_) do
    Repo.all(from p in Peer, select: p)
  end
  def peers() do
    Repo.all(from p in Peer, select: p)
  end

  def peer_for_address(_swarm = %__MODULE__{}, {ip, port}) do
    Repo.one(from p in Peer, where: p.address == ^ip and p.port == ^port)
  end

  def add(swarm = %__MODULE__{}, peers) do
    changesets = Enum.map(peers, fn peer ->
      %{
        address: %Postgrex.INET{address: peer.ip},
        port: peer.port,
        peer_id: Map.get(peer, :peer_id, nil)
      }
    end)

    Repo.insert_all(Peer, changesets, on_conflict: {:replace, [:address]},
                   conflict_target: [:peer_id])
    swarm
  end

  def valid_peer?(%{port: port}) when port in 1..65_535, do: true
  def valid_peer?(_), do: false

  def requested_blocks(swarm) do
    Repo.all(from request in Request,
              join: block in assoc(request, :block),
              join: peer in assoc(request, :peer),
              join: piece in assoc(block, :piece),
              join: torrent in assoc(piece, :torrent),
              where: torrent.info_hash == ^swarm.info_hash,
              select: {peer.peer_id, %{index: piece.index}})
  end

  # requests already made: {block => [peer_id]}
  # Consider this as an inversion of the peer => [block_id] map
  def get_request_peers(swarm) do
    swarm.requested_block_peers
  end

  def cancel_block_requests(swarm, block, from) when is_peer_id(from) do
    peer_ids_to_send_cancel = Repo.all(from request in Request,
                                        join: block in assoc(request, :block),
                                        join: piece in assoc(block, :piece),
                                        join: peer in assoc(request, :peer),
                                        where: piece.index == ^block.index
                                           and block.offset == ^block.offset
                                           and peer.peer_id != ^from,
                                        select: {peer.peer_id, piece.index, block.offset, block.size})

    cancel_messages =
      peer_ids_to_send_cancel
      |> Enum.map(fn {peer_id, index, offset, size} ->
        {:btp_send, peer_id, {:cancel, index, offset, size}}
      end)
      |> Enum.uniq()

    Repo.delete_all(from request in Request,
                      join: block in assoc(request, :block),
                      join: piece in assoc(block, :piece),
                      join: peer in assoc(request, :peer),
                      where: piece.index == ^block.index
                         and block.offset == ^block.offset)

    {swarm, cancel_messages}
  end

  def peer_has_max_requests?(swarm, peer_id) do
    max_requests_per_peer = Application.get_env(:effusion, :max_requests_per_peer, 100)
    peer_request_count = count_peer_requests(swarm, peer_id)
    peer_request_count >= max_requests_per_peer
  end

  def count_peer_requests(swarm, peer_id) do
    Repo.aggregate((from request in Request,
                    join: peer in assoc(request, :peer),
                    join: block in assoc(request, :block),
                    join: torrent in assoc(block, :torrent),
                    where: torrent.info_hash == ^swarm.info_hash,
                    where: peer.peer_id == ^peer_id),
                   :count, :id)
  end

  def get_connected_peer(_swarm = %__MODULE__{}, remote_peer_id)
      when is_peer_id(remote_peer_id) do
    peer_query = from peer in Peer,
                  where: peer.peer_id == ^remote_peer_id
    case Repo.one(peer_query) do
      {:ok, peer} -> peer
      _ -> {:error, :peer_not_found}
    end
  end

  def handle_connect(swarm = %__MODULE__{}, peer_id, {ip, port}) when is_peer_id(peer_id) do
    # cases:
    # - another peer (difference peer_id) has that address
    # - peer with peer_id has a different address
    # - peer with peer_id has same address
    # - peer is new

    conflicting_peers_query = from p in Peer,
                              where: p.address == ^%Postgrex.INET{address: ip}
                                and p.port == ^port
                                and p.peer_id != ^peer_id

    Repo.delete_all(conflicting_peers_query)

    %Peer{
      peer_id: peer_id,
      address: %Postgrex.INET{address: ip},
      port: port
    }
    |> Peer.changeset()
    |> Repo.insert!(on_conflict: {:replace, [:address, :port, :peer_id]},
                                  conflict_target: [:address, :port])
    swarm
  end

  def handle_disconnect(swarm = %__MODULE__{}, {ip, port}, _reason \\ :normal) do
    addr = %Postgrex.INET{address: ip}
    peer = Repo.one(from peer in Peer,
                            where: peer.address == ^addr
                            and peer.port == ^port)
    peer = Peer.changeset(peer)

    Repo.update(peer,
                update: [inc: [failcount: 1]])
    swarm
  end
end
