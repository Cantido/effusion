defmodule Effusion.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    :ets.new(MetadataTable, [:set, :public, :named_table, read_concurrency: true])

    children = [
      Effusion.Application.SessionServerSupervisor,
      Effusion.Application.ConnectionSupervisor,
      {Registry, keys: :duplicate, name: ConnectionRegistry},
      {Registry, keys: :unique, name: SessionRegistry},
      Effusion.PieceStage,
      Effusion.IOStage
    ]

    opts = [strategy: :one_for_one, name: Effusion.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
