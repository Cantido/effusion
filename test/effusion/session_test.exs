defmodule Effusion.SessionTest do
  use ExUnit.Case
  doctest Effusion.Session
  alias Effusion.Metainfo
  import Mox

  setup :verify_on_exit!

  test "initializes correctly" do
    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    peer_id = "Effusion Experiment!"
    ip = {127, 0, 0, 1}
    port = 4040

    {:ok, state} = Effusion.Session.init(metabin, peer_id, ip, port)

    {:ok, meta} = Metainfo.decode(metabin)

    assert state.meta == meta
    assert state.ip == ip
    assert state.port == port
    assert state.peer_id == peer_id
  end

  defp stub_tracker(_a, _b, _c, _d, _e, _f, _g, _h) do
      {
        :ok,
        %{
          interval: 9_000,
          peers: [%{ip: "192.168.1.1", port: 7001}]
        }
      }
    end

  test "calls tracker" do
    Effusion.THP.Mock |> expect(:announce, &stub_tracker/8)

    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    Effusion.Session.start metabin
  end
end
