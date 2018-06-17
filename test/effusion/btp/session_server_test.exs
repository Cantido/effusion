defmodule Effusion.BTP.SessionServerTest do
  use ExUnit.Case
  alias Effusion.BTP.SessionServer
  import Mox
  doctest Effusion.BTP.SessionServer

  setup :verify_on_exit!
  setup :set_mox_global

  defp stub_started(_, _, _, _, _, _, _, _, event) do
    assert event == :started
    {:ok, %{interval: 1, peers: []}}
  end
  defp stub_interval(_, _, _, _, _, _, _, _, event) do
    assert event == :interval
    {:ok, %{interval: 9_000, peers: []}}
  end
  defp stub_stopped(_, _, _, _, _, _, _, _, event) do
    assert event == :stopped
    {:ok, %{interval: 9_000, peers: []}}
  end

  test "announces again after interval" do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/9)
    |> expect(:announce, &stub_interval/9)

    {:ok, _} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, nil]
    })

    :timer.sleep(1_100)
  end

  test "sends STOPPED event on termination" do
    Effusion.THP.Mock
    |> expect(:announce, &stub_started/9)
    |> expect(:announce, &stub_stopped/9)

    {:ok, spid} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, nil]
    })

    GenServer.stop(spid)
    :timer.sleep(100)
    verify!()
  end
end
