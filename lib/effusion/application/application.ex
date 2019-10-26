defmodule Effusion.Application do
  @moduledoc false

  use Application

  @listen_port Application.get_env(:effusion, :server_port)

  def start(_type, _args) do
    :ets.new(MetadataTable, [:set, :public, :named_table, read_concurrency: true])

    children = [
      Effusion.Application.DownloadServerSupervisor,
      Effusion.Application.ConnectionSupervisor,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: SessionRegistry}
    ]

    {:ok, listener} = :ranch.start_listener(:pwp, 100, :ranch_tcp, [port: 8001], Effusion.PWP.Handler, [])

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
