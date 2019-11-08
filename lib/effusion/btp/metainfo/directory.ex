defmodule Effusion.BTP.Metainfo.Directory do
  use GenServer

  @moduledoc """
  A lookup table of a torrent's metainfo.
  """

  def start_link(_args) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def lookup(info_hash) do
    GenServer.call(__MODULE__, {:lookup, info_hash})
  end

  def insert(metainfo) do
    GenServer.call(__MODULE__, {:insert, metainfo})
  end

  def init(:ok) do
    {:ok, %{}}
  end

  def handle_call({:lookup, info_hash}, _from, state) do
    {:reply, Map.get(state, info_hash), state}
  end

  def handle_call({:insert, metainfo}, _from, state) do
    {:reply, :ok, Map.put(state, metainfo.info_hash, metainfo)}
  end
end
