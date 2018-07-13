defmodule Effusion.BTP.SessionServerTest do
  use ExUnit.Case
  alias Effusion.BTP.SessionServer
  import Mox
  doctest Effusion.BTP.SessionServer

  setup :verify_on_exit!
  setup :set_mox_global

  defp stub_started(_, _, _, _, _, _, _, _, event, _) do
    assert event == :started
    {:ok, %{interval: 1, peers: []}}
  end
  defp stub_interval(_, _, _, _, _, _, _, _, event, _) do
    assert event == :interval
    {:ok, %{interval: 9_000, peers: []}}
  end
  defp stub_stopped(_, _, _, _, _, _, _, _, event, _) do
    assert event == :stopped
    {:ok, %{interval: 9_000, peers: []}}
  end
  defp stub_completed(_, _, _, _, _, _, _, _, event, _) do
    assert event == :completed
    {:ok, %{interval: 9_000, peers: []}}
  end

  setup do
    Temp.track!

    {:ok, file} = Temp.path

    on_exit fn ->
      File.rm_rf file
    end

    %{destfile: file}
  end

  test "announces again after interval", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/10)
    |> expect(:announce, &stub_interval/10)

    {:ok, _} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, file]
    })

    :timer.sleep(1_100)
  end

  test "sends STOPPED event on termination", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/10)
    |> expect(:announce, &stub_stopped/10)

    {:ok, spid} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, file]
    })

    GenServer.stop(spid)
    :timer.sleep(100)
    verify!()
  end

  test "sends COMPLETED event and terminates when finished", %{destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/10)
    |> expect(:announce, &stub_completed/10)

    {:ok, spid} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, file]
    })

    ih = TestHelper.tiny_meta().info_hash

    SessionServer.handle_message(ih, "peer id ~~~~~~~~~~~~", {:piece, %{index: 0, offset: 0, data: "tin"}})
    SessionServer.handle_message(ih, "peer id ~~~~~~~~~~~~", {:piece, %{index: 1, offset: 0, data: "y\n"}})

    :timer.sleep(100)

    refute Process.alive?(spid)
  end
end
