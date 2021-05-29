defmodule Effusion.PWP.Swarm do

  alias Effusion.PWP.Swarm.Commands.AddPeerAddress
  alias Effusion.Commanded, as: CQRS

  @doc """
  Add a new peer to possibly connect to.
  """
  def add(info_hash, host, port, from) do
    %AddPeerAddress{
      peer_uuid: UUID.uuid4(),
      expected_info_hash: Effusion.Hash.encode(info_hash),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from
    }
    |> CQRS.dispatch()
  end

  @doc """
  Add a new peer to possibly connect to.
  """
  def add(peer_uuid, info_hash, host, port, from) do
    %AddPeerAddress{
      peer_uuid: peer_uuid,
      expected_info_hash: Effusion.Hash.encode(info_hash),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from
    }
    |> CQRS.dispatch()
  end

  @doc """
  Add a new peer to possibly connect to.
  """
  def add(peer_uuid, info_hash, peer_id, host, port, from) do
    %AddPeerAddress{
      peer_uuid: peer_uuid,
      expected_info_hash: Effusion.Hash.encode(info_hash),
      expected_peer_id: Effusion.Hash.encode(peer_id),
      host: to_string(:inet.ntoa(host)),
      port: port,
      from: from
    }
    |> CQRS.dispatch()
  end
end
