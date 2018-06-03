defmodule Effusion.BTP.Peer do
  alias Effusion.PWP.Messages
  alias Effusion.Application.SessionServer

  def new({_host, _port} = address, peer_id, info_hash, session) when is_pid(session) do
    %{
      address: address,
      peer_id: peer_id,
      remote_peer_id: nil,
      info_hash: info_hash,
      session: session,
      handshaken: false,
      peer_choking: true,
      peer_interested: false,
      am_choking: true,
      am_interested: false,
      has: IntSet.new()
    }
  end

  def get_handshake(p) do
    {:handshake, p.peer_id, p.info_hash}
  end

  def handshake(p, {:handshake, remote_peer_id, info_hash, _reserved}) do
    cond do
      p.handshaken ->
        {:error, :local_peer_already_handshaken}
      p.info_hash != info_hash ->
        {:error, :mismatched_info_hash, [expected: p.info_hash, actual: info_hash]}
      true ->
        {:ok, %{p | handshaken: true, remote_peer_id: remote_peer_id}}
    end
  end

  def recv(p, {:bitfield, b}) do
    p = p
      |> Map.put(:has, IntSet.new(b))
      |> Map.put(:am_choking, false)
      |> Map.put(:am_interested, true)

    {p, [:interested, :unchoke]}
  end

  def recv(p, {:piece, b}) do
    SessionServer.block(p.session, b)
    {
      p,
      requests(p)
    }
  end

  def recv(p, :unchoke) do
    {
      Map.put(p, :peer_choking, false),
      requests(p)
    }
  end

  def recv(p, {:have, i}) do
    {
      Map.update!(p, :has, &IntSet.put(&1, i)),
      []
    }
  end

  def recv(p, _) do
    {p, []}
  end

  defp requests(p) do
    case SessionServer.next_request(p.session) do
      %{index: i, offset: o, size: s} -> [{:request, i, o, s}]
      :done -> []
    end
  end
end
