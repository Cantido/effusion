defmodule Effusion.SessionTest do
  use ExUnit.Case
  doctest Effusion.Session
  alias Effusion.Session
  import Mox

  setup :verify_on_exit!

  @peer_id "EffusionSessionTest!"
  @ip {10, 0, 0, 5}
  @port 8999
  @info_hash TestHelper.mint_info_hash()
  @left 1899528192

  setup context do
    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    {:ok, server} = start_supervised {Effusion.Session, [metabin, @peer_id , @ip, @port]}
    {:ok, Map.merge(context, %{server: server, metabin: metabin})}
  end

  defp stub_tracker(url, ip, port, peer_id, info_hash, up, down, left) do
    assert url == "https://torrents.linuxmint.com/announce.php"
    assert ip == @ip
    assert port == @port
    assert peer_id == @peer_id
    assert info_hash == @info_hash
    assert up == 0
    assert down == 0
    assert left == @left
    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: "192.168.1.1", port: 7001}]
      }
    }
  end

  test "calls tracker and selects peers", %{server: server} do
    allow(Effusion.THP.Mock, self(), server)
    Effusion.THP.Mock |> expect(:announce, &stub_tracker/8)

    :ok = Session.announce(server)
    {:ok, peer} = Session.select_peer(server)

    assert peer == %{ip: "192.168.1.1", port: 7001}
  end
end
