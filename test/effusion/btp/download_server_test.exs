defmodule Effusion.BTP.DownloadServerTest do
  use ExUnit.Case
  alias Effusion.BTP.DownloadServer
  import Mox
  doctest Effusion.BTP.DownloadServer

  setup :verify_on_exit!
  setup :set_mox_global

  defp stub_started(_, _, _, _, _, _, _, _, opts) do
    assert Keyword.get(opts, :event, :started)
    {:ok, %{interval: 1, peers: []}}
  end

  defp stub_interval(_, _, _, _, _, _, _, _, opts) do
    assert Keyword.get(opts, :event, :interval)
    {:ok, %{interval: 9_000, peers: []}}
  end

  defp stub_stopped(_, _, _, _, _, _, _, _, opts) do
    assert Keyword.get(opts, :event, :stopped)
    {:ok, %{interval: 9_000, peers: []}}
  end

  defp stub_completed(_, _, _, _, _, _, _, _, opts) do
    assert Keyword.get(opts, :event, :completed)
    {:ok, %{interval: 9_000, peers: []}}
  end

  setup do
    Temp.track!()

    {:ok, file} = Temp.path()

    on_exit(fn ->
      File.rm_rf(file)
    end)

    %{destfile: file}
  end

  test "announces again after interval", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/9)
    |> expect(:announce, &stub_interval/9)

    {:ok, _} =
      start_supervised({
        DownloadServer,
        [TestHelper.tiny_meta(), {nil, nil}, file]
      })

    :timer.sleep(1_100)
  end

  test "sends STOPPED event on termination", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/9)
    |> expect(:announce, &stub_stopped/9)

    {:ok, spid} =
      start_supervised({
        DownloadServer,
        [TestHelper.tiny_meta(), {nil, nil}, file]
      })

    GenServer.stop(spid)
    :timer.sleep(100)
    verify!()
  end

  test "sends COMPLETED event and terminates when finished", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/9)
    |> expect(:announce, &stub_completed/9)

    {:ok, spid} =
      start_supervised({
        DownloadServer,
        [TestHelper.tiny_meta(), {nil, nil}, file]
      })

    ih = TestHelper.tiny_meta().info_hash

    DownloadServer.connected(ih, "peer id ~~~~~~~~~~~~", {"host", 8000})

    DownloadServer.handle_message(
      ih,
      "peer id ~~~~~~~~~~~~",
      {:piece, %{index: 0, offset: 0, data: "tin"}}
    )

    DownloadServer.handle_message(
      ih,
      "peer id ~~~~~~~~~~~~",
      {:piece, %{index: 1, offset: 0, data: "y\n"}}
    )

    :timer.sleep(100)

    refute Process.alive?(spid)
  end
end
