defmodule Effusion.Statistics.PeerDownloadAverage do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def peer_20sec_download_avg(peer_id) do
    GenServer.call(__MODULE__, {:peer_20sec_download_avg, peer_id})
  end

  def init(:ok) do
    Process.send_after(self(), :accumulate, 1_000)
    {:ok, %{}}
  end

  def handle_call({:peer_20sec_download_avg, peer_id}, _from, state) do
    peer_stats = Map.get(state, peer_id)
    if peer_stats != nil do
      bytes_per_second = peer_stats.bytes_per_second
      {:reply, avg(bytes_per_second), state}
    else
      {:reply, 0, state}
    end
  end

  def handle_info(:accumulate, state) do
    Process.send_after(self(), :accumulate, 1_000)
    state = :ets.foldl(fn {peer_id, bytes}, acc ->
      Map.update(acc, peer_id, %{last_total_seen: bytes, bytes_per_second: [bytes, 0]}, &update_peer_speed(&1, bytes))
    end, state, PeerDownloadStatsTable)
    {:noreply, state}
  end

  def update_peer_speed(%{last_total_seen: last_total_seen, bytes_per_second: bytes_per_second}, new_total_seen) do
    bytes_this_second = new_total_seen - last_total_seen
    new_bytes_per_second = [bytes_this_second | List.delete_at(bytes_per_second, 19)]

    %{
      last_total_seen: new_total_seen,
      bytes_per_second: new_bytes_per_second
    }
  end

  defp avg(list) when is_list(list) and length(list) > 0 do
    Enum.sum(list) / length(list)
  end

  defp avg(list) when is_list(list) and length(list) == 0 do
    0
  end
end
