defmodule Effusion.Factory do
  alias Effusion.Repo

  def encoded_info_hash do
    info_hash() |> Effusion.Hash.encode()
  end

  def encoded_peer_id do
    peer_id() |> Effusion.Hash.encode()
  end

  def encoded_node_id do
    node_id() |> Effusion.Hash.encode()
  end

  def info_hash do
    :crypto.strong_rand_bytes(20)
  end

  def peer_id do
    :crypto.strong_rand_bytes(20)
  end

  def node_id do
    :crypto.strong_rand_bytes(20)
  end
end
