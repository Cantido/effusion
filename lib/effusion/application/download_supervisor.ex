defmodule Effusion.Application.DownloadSupervisor do
  use Supervisor
  alias Effusion.BTP.{
    DownloadSpeedWatcher,
    ProtocolHandler
  }
  alias Effusion.THP.Announcer

  @moduledoc """
  A supervisor that supervises all per-download processes.
  """

  def start_link(info_hash) do
    Supervisor.start_link(__MODULE__, info_hash)
  end

  @impl true
  def init(info_hash) do
    children = [
      {Announcer, info_hash},
      {DownloadSpeedWatcher, info_hash},
      {ProtocolHandler, info_hash}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end