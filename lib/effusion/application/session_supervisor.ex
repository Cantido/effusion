defmodule Effusion.Application.SessionServerSupervisor do
  use DynamicSupervisor

  def start_link([]) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def start_child([meta, _local_peer, _file] = args) do
    spec = %{
      id: meta.info_hash,
      start: {Effusion.BTP.DownloadServer, :start_link, [args]}
    }
    DynamicSupervisor.start_child(__MODULE__, spec)
  end
end
