defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion
  import Mox

  @peer_id "EffusionSessionTest!"
  @ip {10, 0, 0, 5}
  @port 8999
  @info_hash TestHelper.mint_info_hash()
  @left 1899528192

  setup :verify_on_exit!
  setup :set_mox_global

  setup context do
    {:ok, metabin} = File.read "test/linuxmint-18.3-cinnamon-64bit.iso.torrent"
    {:ok, Map.put(context, :metabin, metabin)}
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
        peers: [%{ip: {192, 168, 1, 1}, port: 7001, peer_id: "another peer 1234567"}]
      }
    }
  end

  defp stub_tcp(host, port, []) do
    assert host == {192, 168, 1, 1}
    assert port == 7001
    {:ok, {}}
  end

  defp stub_send(_socket, _bin) do
    :ok
  end

  defp stub_recv(_socket, 0) do
    {:ok, :unchoke}
  end

  defp stub_recv(_socket, 68) do
    {:ok, Effusion.PWP.Messages.Handshake.encode("Remote Peer 76543210", @info_hash)}
  end

  test "adding a torrent announces to a tracker and connects to a peer", %{metabin: metabin} do
    Effusion.THP.Mock |> expect(:announce, &stub_tracker/8)
    Effusion.Transport.Mock |> expect(:connect, &stub_tcp/3)
    Effusion.Transport.Mock |> expect(:send, &stub_send/2)
    Effusion.Transport.Mock |> expect(:recv, &stub_recv/2)
    Effusion.Transport.Mock |> expect(:recv, &stub_recv/2)
    {:ok, _session} = Effusion.add_torrent(metabin, @peer_id, @ip, @port)

    # this pauses the process just enough to let the genserver process messages
    :timer.sleep(1)
  end
end
