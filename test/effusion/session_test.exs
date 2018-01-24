defmodule Effusion.SessionTest do
  use ExUnit.Case
  doctest Effusion.Session
  alias Effusion.Session
  import Mox

  setup :verify_on_exit!
  setup :set_mox_global

  @peer_id "EffusionSessionTest!"
  @ip {10, 0, 0, 5}
  @port 8999
  @info_hash TestHelper.mint_info_hash()
  @left 1899528192

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


  defp stub_tcp(host, port, []) do
    assert host == {192, 168, 1, 1}
    assert port == 7001
    {:ok, {}}
  end

  test "calls tracker and selects peers" do
    Effusion.THP.Mock |> expect(:announce, &stub_tracker/8)
    Effusion.Transport.Mock |> expect(:connect, &stub_tcp/3)

    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    {:ok, _server} = start_supervised {Session, [metabin, @peer_id , @ip, @port]}

    :timer.sleep(1) # lets the out-of-process handshake to complete
  end
end
