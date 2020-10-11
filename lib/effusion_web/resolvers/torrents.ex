defmodule EffusionWeb.Resolvers.Torrents do
  alias Effusion.Repo

  @moduledoc """
  Absinthe resolvers for torrents.
  """

  def all_torrents(_root, _args, _info) do
    raise "TODO"
  end

  def find_torrent(_parent, %{id: id}, _resolution) do
    raise "TODO"
  end

  def downloaded(torrent, _args, _info) do
    raise "TODO"
  end

  def connected_peers_count(torrent, _args, _info) do
    raise "TODO"
  end

  def available_peers_count(torrent, _args, _info) do
    raise "TODO"
  end

  def add_torrent(_root, %{meta: meta}, _info) do
    raise "TODO"
  end

  def pause_torrent(_root, %{id: id}, _info) do
    raise "TODO"
  end

  def start_torrent(_root, %{id: id}, _info) do
    raise "TODO"
  end
end
