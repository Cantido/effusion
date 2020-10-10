defmodule Effusion.BTP.Peer do
  alias Effusion.BTP.Torrent
  import Effusion.Hash, only: [is_hash: 1]
  import Ecto.Changeset
  import Ecto.Query
  use Ecto.Schema

  @moduledoc """
  Functions for manipulating a member of a BitTorrent swarm.
  """

  @doc """
  Returns `true` if `term` is a 20-byte binary peer ID; `false` otherwise.

  Allowed in guard tests.

  ## Examples

      iex> Effusion.BTP.Peer.is_peer_id("12345678901234567890")
      true

      iex> Effusion.BTP.Peer.is_peer_id("1234567890")
      false
  """
  defguard is_peer_id(term) when not is_nil(term) and is_binary(term) and byte_size(term) == 20

  @required_fields [
    :address,
    :port
  ]

  @optional_fields [
    :torrent_id,
    :peer_id,
    :failcount,
    :peer_choking,
    :peer_interested,
    :am_choking,
    :am_interested,
    :connected,
    :fast_extension
  ]

  schema "peers" do
    belongs_to :torrent, Torrent
    field :peer_id, :binary, null: true
    field :address, EctoNetwork.INET, null: false
    field :port, :integer, null: false
    field :failcount, :integer, default: 0, null: false
    field :peer_choking, :boolean, default: true, null: false
    field :peer_interested, :boolean, default: false, null: false
    field :am_choking, :boolean, default: true, null: false
    field :am_interested, :boolean, default: false, null: false
    field :connected, :boolean, default: false, null: false
    field :fast_extension, :boolean, default: false, null: false
    has_many :blocks_we_requested, Effusion.BTP.Request
    has_many :peer_pieces, Effusion.BTP.PeerPiece
    has_many :pieces, through: [:peer_pieces, :piece]
  end

  @doc """
  Create a new peer data structure.
  """
  def new({_host, port} = address, peer_id \\ nil) when port > 0 and (is_peer_id(peer_id) or is_nil(peer_id)) do
    %__MODULE__{address: address, peer_id: peer_id, failcount: 0}
  end

  def get(info_hash, {ip, port}) do
    from peer in __MODULE__,
    join: torrent in assoc(peer, :torrent),
    where: torrent.info_hash == ^info_hash,
    where: peer.address == ^%Postgrex.INET{address: ip},
    where: peer.port == ^port
  end

  def connected_query(info_hash) when is_hash(info_hash) do
    from peer in __MODULE__,
    join: torrent in assoc(peer, :torrent),
    where: torrent.info_hash == ^info_hash,
    where: peer.connected
  end

  def all(info_hash) when is_hash(info_hash) do
    from peer in __MODULE__,
    join: torrent in assoc(peer, :torrent),
    where: torrent.info_hash == ^info_hash
  end

  def changeset(peer, params \\ %{}) do
    peer
    |> cast(params, @required_fields ++ @optional_fields)
    |> cast_assoc(:torrent, with: &Torrent.changeset/2)
    |> validate_required(@required_fields)
    |> validate_number(:port, greater_than: 0)
    |> check_constraint(:port, name: :port_must_be_positive)
    |> unique_constraint(:peer_id, name: "peers_torrent_id_peer_id_index")
    |> unique_constraint(:address, name: "peers_address_port_index")
  end

  def compact(peer) do
    {ip0, ip1, ip2, ip3} = peer.address.address
    port = peer.port
    <<ip0, ip1, ip2, ip3, port::16>>
  end
end
