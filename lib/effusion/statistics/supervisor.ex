defmodule Effusion.Statistics.Supervisor do
  use Supervisor

  @moduledoc """
  Supervises a dynamic number of `Effusion.BTP.ProtocolHandler` processes.
  """

  def start_link([]) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      Effusion.Statistics.PeerDownloadAverage,
      Effusion.Statistics.SessionDownloadAverage,
    ]
    Supervisor.init(children, strategy: :one_for_one)
  end
end
