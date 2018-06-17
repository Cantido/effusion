defmodule Effusion.BTP.SessionServerTest do
  use ExUnit.Case
  alias Effusion.BTP.SessionServer
  import Mox
  doctest Effusion.BTP.SessionServer

  setup :verify_on_exit!
  setup :set_mox_global

  defp stub_tracker(_, _, _, _, _, _, _, _, _) do
    {:ok, %{interval: 1, peers: []}}
  end

  test "announces again after interval" do
    Effusion.THP.Mock
    |> expect(:announce, &stub_tracker/9)
    |> expect(:announce, &stub_tracker/9)

    {:ok, _} = start_supervised({
      SessionServer,
      [TestHelper.tiny_meta(), {nil, nil}, nil]
    })

    :timer.sleep(1_100)
  end
end
