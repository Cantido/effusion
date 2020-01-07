defmodule Effusion.Application.DownloadSupervisor do
  use Supervisor
  alias Effusion.BTP.{
    ProtocolHandler,
    VerifierWatchdog,
    DownloadSpeedWatcher
  }
  alias Effusion.THP.Announcer

  def start_link(info_hash) do
    Supervisor.start_link(__MODULE__, info_hash)
  end

  @impl true
  def init(info_hash) do
    children = [
      {VerifierWatchdog, info_hash},
      {Announcer, info_hash},
      {DownloadSpeedWatcher, info_hash},
      {ProtocolHandler, info_hash}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
