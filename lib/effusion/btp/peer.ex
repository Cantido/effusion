defmodule Effusion.BTP.Peer do
  alias Effusion.PWP.Messages

  def new({_host, _port} = address, peer_id, info_hash, session) do
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
      am_interested: false
    }
  end

  def address(p) do
    p.address
  end

  def get_handshake(p) do
    Messages.encode({:handshake, p.peer_id, p.info_hash})
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

  def is_not_choking(p) do
    %{p | am_choking: false}
  end

  def is_not_choked(p) do
    %{p | peer_choking: false}
  end

  def is_interested(p) do
    %{p | am_interested: true}
  end

  def is_not_interested(p) do
    %{p | am_interested: false}
  end

  def recv_bitfield(p) do
    p = p
      |> is_not_choking()
      |> is_interested()

    {p, [:interested, :unchoke]}
  end

  def recv_unchoke(p) do
    is_not_choked(p)
  end
end
