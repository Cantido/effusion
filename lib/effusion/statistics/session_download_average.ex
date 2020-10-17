defmodule Effusion.Statistics.SessionDownloadAverage do
  alias Effusion.Statistics.Net, as: NetStats
  use GenServer

  @moduledoc """
  Tracks the overall download speed for the session.
  """

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Get the overall download speed for the current session.
  """
  def session_20sec_download_avg do
    GenServer.call(__MODULE__, :session_20sec_download_avg)
  end

  def session_01sec_download_avg do
    GenServer.call(__MODULE__, :session_01sec_download_avg)
  end

  @doc """
  Initialize the download speed tracker.
  """
  def init(:ok) do
    Process.send_after(self(), :accumulate, 1_000)
    {:ok, %{last_total_seen: 0, bytes_per_second: [0]}}
  end

  def handle_call(:session_20sec_download_avg, _from, state) do
    {:reply, avg(state.bytes_per_second), state}
  end

  def handle_call(:session_01sec_download_avg, _from, state) do
    {:reply, state.bytes_per_second |> Enum.at(0), state}
  end

  def handle_info(:accumulate, state) do
    Process.send_after(self(), :accumulate, 1_000)
    total = NetStats.recv_bytes()
    state = update_speed(state, total)
    {:noreply, state}
  end

  defp update_speed(%{last_total_seen: last_total_seen, bytes_per_second: bytes_per_second}, new_total_seen) do
    bytes_this_second = new_total_seen - last_total_seen
    new_bytes_per_second = [bytes_this_second | List.delete_at(bytes_per_second, 19)]

    %{
      last_total_seen: new_total_seen,
      bytes_per_second: new_bytes_per_second
    }
  end

  defp avg(list) when is_list(list) and length(list) > 0 do
    # arithmetic mean
    Enum.sum(list) / length(list)
  end

  defp avg([]) do
    0
  end
end
