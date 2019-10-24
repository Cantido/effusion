defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion
  alias Effusion.BTP.Peer
  alias Effusion.PWP.Socket
  import Mox

  setup :verify_on_exit!
  setup :set_mox_global

  @torrent %{
    announce: "http://localhost:6969/announce",
    created_by: "Lovingly hand-crafted, by Rosa <3",
    creation_date: 1_517_785_476,
    encoding: "UTF-8",
    info: %{
      :length => 5,
      :name => "tiny.txt",
      :piece_length => 3,
      # Pieces are "tin" and "y\n"
      :pieces => [
        <<242, 105, 25, 118, 134, 197, 108, 67, 163, 82, 84, 216, 119, 167, 25, 148, 192, 181,
          112, 48>>,
        <<144, 99, 169, 240, 224, 50, 182, 35, 148, 3, 183, 25, 203, 187, 165, 106, 196, 228, 228,
          95>>
      ],
      "private" => 0
    },
    info_hash:
      <<95, 189, 143, 1, 37, 56, 146, 40, 140, 78, 2, 250, 208, 144, 217, 10, 49, 7, 64, 28>>
  }

  @remote_peer Peer.new(
                 {{127, 0, 0, 1}, 8001},
                 "Other peer 123456789",
                 @torrent.info_hash
               )

  defp stub_tracker(_, _, _, _, _, _, _, _, _, _) do
    {host, port} = @remote_peer.address

    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: host, port: port, peer_id: @remote_peer.peer_id}]
      }
    }
  end

  setup do
    {_host, port} = @remote_peer.address
    {:ok, lsock} = Socket.listen(port)

    on_exit(fn ->
      :ok = Socket.close(lsock)
    end)

    %{lsock: lsock}
  end

  setup do
    Temp.track!()

    {:ok, file} = Temp.path()

    on_exit(fn ->
      File.rm_rf(file)
    end)

    %{destfile: file}
  end

  setup do
    Application.put_env(:effusion, :server_address, {{127, 0, 0, 1}, 8000})
  end

  test "download a file", %{lsock: lsock, destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, 2, &stub_tracker/10)

    {:ok, _} = Effusion.start_download(@torrent, file)

    {:ok, sock, _remote_peer} = Socket.accept(lsock, @remote_peer)
    bitfield = IntSet.new([0, 1]) |> IntSet.bitstring()
    :ok = Socket.send_msg(sock, {:bitfield, bitfield})
    {:ok, :interested} = Socket.recv(sock)
    {:ok, :unchoke} = Socket.recv(sock)
    :ok = Socket.send_msg(sock, :unchoke)

    {:ok, {:request, %{index: i1}}} = Socket.recv(sock)

    {i2, piece1, piece2} =
      case i1 do
        0 -> {1, {:piece, 0, 0, "tin"}, {:piece, 1, 0, "y\n"}}
        1 -> {0, {:piece, 1, 0, "y\n"}, {:piece, 0, 0, "tin"}}
      end

    :ok = Socket.send_msg(sock, piece1)
    {:ok, {:have, ^i1}} = Socket.recv(sock)
    {:ok, {:request, %{index: ^i2}}} = Socket.recv(sock)

    :ok = Socket.send_msg(sock, piece2)
    {:ok, {:have, ^i2}} = Socket.recv(sock)

    :timer.sleep(100)
    :file.datasync(file)

    {:ok, contents} = File.read(Path.join(file, "tiny.txt"))

    assert "tiny\n" == contents
  end
end
