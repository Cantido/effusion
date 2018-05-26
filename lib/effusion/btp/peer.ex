defmodule Effusion.BTP.Peer do
  alias Effusion.PWP.Messages.Handshake

  def new({_host, _port} = address, peer_id, info_hash, session) do
    %{
      address: address,
      peer_id: peer_id,
      info_hash: info_hash,
      session: session,
      peer_choking: true,
      peer_interested: false,
      am_choking: true,
      am_interested: false
    }
  end

  def address(p) do
    p.address
  end

  def handshake(p) do
    Handshake.encode(p.peer_id, p.info_hash)
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
