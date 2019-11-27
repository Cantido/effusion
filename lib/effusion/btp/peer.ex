defmodule Effusion.BTP.Peer do
  alias Effusion.PWP.ConnectionRegistry
  alias Effusion.Repo
  import Effusion.Hash
  import Ecto.Changeset
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

  schema "peers" do
    field :peer_id, :binary, null: true
    field :address, EctoNetwork.INET, null: false
    field :port, :integer, null: false
    field :failcount, :integer, default: 0, null: false
    field :peer_choking, :boolean, default: true, null: false
    field :peer_interested, :boolean, default: false, null: false
    field :am_choking, :boolean, default: true, null: false
    field :am_interested, :boolean, default: false, null: false
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

  def connected?(peer, info_hash) do
    ConnectionRegistry.connected?(info_hash, peer.peer_id)
  end

  def changeset(peer, params \\ %{}) do
    peer
    |> cast(params, [:address, :port, :peer_id, :failcount, :peer_choking, :peer_interested, :am_choking, :am_interested])
    |> unique_constraint(:peer_id)
    |> unique_constraint(:address, name: "peers_address_port_index")
  end
end
