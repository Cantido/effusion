defmodule EffusionTest do
  use ExUnit.Case
  doctest Effusion
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Socket
  import Mox

  setup :verify_on_exit!
  setup :set_mox_global

  @torrent %{
    announce: "http://localhost:6969/announce",
    created_by: "Lovingly hand-crafted, by Rosa <3",
    creation_date: 1517785476,
    encoding: "UTF-8",
    info: %{
      :length => 5,
      :name => "tiny.txt",
      :piece_length => 3,
      # Pieces are "tin" and "y\n"
      :pieces => [
        <<242, 105, 25, 118, 134, 197, 108, 67, 163, 82, 84, 216, 119, 167, 25, 148, 192, 181, 112, 48>>,
        <<144, 99, 169, 240, 224, 50, 182, 35, 148, 3, 183, 25, 203, 187, 165, 106, 196, 228, 228, 95>>
    ],
      "private" => 0
    },
    info_hash: <<95, 189, 143, 1, 37, 56, 146, 40, 140, 78, 2, 250, 208, 144, 217, 10, 49, 7, 64, 28>>
  }

  @local_peer %{
    id: "Effusion Experiment!",
    host: {127, 0, 0, 1},
    port: 8000
  }

  @remote_peer %{
    id: "Other peer 123456789",
    host: {127, 0, 0, 1},
    port: 8001
  }

  defp stub_tracker(_, _, _, _, _, _, _, _) do
    {
      :ok,
      %{
        interval: 9_000,
        peers: [%{ip: @remote_peer.host, port: @remote_peer.port, peer_id: @remote_peer.id}]
      }
    }
  end

  setup do
    {:ok, lsock} = :gen_tcp.listen(@remote_peer.port, [:binary, active: false, reuseaddr: true, send_timeout: 5_000])

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  setup do
    {:ok, file} = File.open("testtiny.txt", [:read, :write])

    on_exit fn ->
      File.rm("testtiny.txt")
    end

    %{destfile: file}
  end

  setup do
    Application.put_env(:effusion, :server_address, {@local_peer.host, @local_peer.port})
  end

  test "download a file", %{lsock: lsock, destfile: file} do
    Effusion.THP.Mock
    |> expect(:announce, &stub_tracker/8)

    {:ok, _} = Effusion.start_download(@torrent, file)

    # Effusion should get our remote peer from the tracker, and try to handshake with it.
    {:ok, sock} = :gen_tcp.accept(lsock, 5_000)
    {:ok, _handshake_packet} = :gen_tcp.recv(sock, 68)
    {:ok, hsbin} = Messages.encode({:handshake, @remote_peer.id, @torrent.info_hash})
    :ok = :gen_tcp.send(sock, hsbin)

    # After the handshake, all subsequent messages have a 4-byte size header
    :ok = :inet.setopts(sock, packet: 4)

    # The remote peer then sends a :bitfield message or :have messages

    # The tiny torrent only has two pieces, and the remote peer has them all
    :ok = Socket.send_msg(sock, {:bitfield, <<0b11>>})

    # Effusion will then send a :interested and :unchoke messages
    {:ok, _interested_packet} = Socket.recv(sock)
    {:ok, _unchoke_packet} = Socket.recv(sock)

    # Then the remote peer will send an unchoke message
    :ok = Socket.send_msg(sock, :unchoke)

    # Effusion will then start requesting pieces, since it is unchoked,
    # and we want to give it the pieces it wants
    {:ok, {:request, %{index: i1}}} = Socket.recv(sock)

    {piece1, piece2} = case i1 do
      0 -> {{:piece, 0, 0, "tin"}, {:piece, 1, 0, "y\n"}}
      1 -> {{:piece, 1, 0, "y\n"}, {:piece, 0, 0, "tin"}}
    end

    :ok = Socket.send_msg(sock, piece1)

    # Effusion will request the next packet, which in our case is the last one.
    {:ok, {:request, _}} = Socket.recv(sock)

    :ok = Socket.send_msg(sock, piece2)

    # The torrent should be completed & verified, and written out to a file.

    :timer.sleep(100)
    :file.datasync(file)

    {:ok, contents} = File.read("testtiny.txt")

    assert "tiny\n" == contents
  end
end
