defmodule Effusion.Factory do
  alias Effusion.Repo

  def encoded_info_hash do
    info_hash() |> Effusion.Hash.encode()
  end

  def encoded_peer_id do
    peer_id() |> Effusion.Hash.encode()
  end

  def info_hash do
    :crypto.strong_rand_bytes(20)
  end

  def peer_id do
    :crypto.strong_rand_bytes(20)
  end

  def build(:torrent) do
    %Effusion.BTP.Torrent{
      info_hash: "12345678901234567890",
      name: "linuxmint-19.2-cinnamon-64bit.iso",
      announce: "http://example.com/announce",
      size: 1_000_000_000,
      piece_size: 1_000_000,
      started: DateTime.utc_now() |> DateTime.truncate(:second),
      comment: "Made with love",
      created_by: "Rosa",
      creation_date: DateTime.utc_now() |> DateTime.truncate(:second)
    }
  end

  def build(:piece) do
    %Effusion.BTP.Piece{
      torrent: build(:torrent),
      index: 0,
      hash: "12345678901234567890",
      size: 5
    }
  end

  def build(:block) do
    %Effusion.BTP.Block{
      piece: build(:piece),
      offset: 0,
      data: "tiny\n",
      size: 5
    }
  end

  def build(factory_name, attributes) do
    factory_name |> build() |> struct(attributes)
  end

  def insert!(factory_name, attributes \\ []) do
    Repo.insert! build(factory_name, attributes)
  end
end
