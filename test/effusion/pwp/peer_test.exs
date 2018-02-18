defmodule Effusion.PWP.PeerTest do
  use ExUnit.Case
  import Mox
  alias Effusion.Session
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Peer

  doctest Effusion.PWP.Peer

  # XXX: In this test, we are acting as the "remote" peer.
  # So, technically, we are "remote", and we are sending stuff to "local".

  @remote_host {127, 0, 0, 1}
  @port 5679
  @remote_address {@remote_host, @port}

  @local_ip {10, 0, 0, 5}
  @local_port 8999
  @local_peer {@local_ip, @local_port}
  @local_peer_id "Effusion Experiment!"
  @remote_peer_id "Other peer 123456789"
  @meta TestHelper.tiny_meta()
  @info_hash @meta.info_hash

  @remote_handshake Handshake.encode(@remote_peer_id, @info_hash)

  defp stub_announce(_, _, _, _, _, _, _, _) do
    {:ok, %{interval: 9_000, peers: []}}
  end

  setup :set_mox_global

  setup do
    Effusion.THP.Mock
    |> stub(:announce, &stub_announce/8)

    {:ok, pid} = start_supervised {Session, [@meta, @local_peer]}
    %{session: pid}
  end

  setup do
    {:ok, lsock} = :gen_tcp.listen(@port, active: false, reuseaddr: true, send_timeout: 5_000)

    on_exit fn ->
      :ok = :gen_tcp.close(lsock)
    end

    %{lsock: lsock}
  end

  test "Peer performs handshake", %{lsock: lsock, session: session} do
    {:ok, _pid} = Peer.connect(@remote_address, @local_peer_id, @info_hash, session)
    {:ok, sock} = :gen_tcp.accept(lsock, 5_000)
    {:ok, handshake_packet} = :gen_tcp.recv(sock, 68)
    :ok = :gen_tcp.send(sock, @remote_handshake)
    :ok = :gen_tcp.close(sock)

    handshake =
      handshake_packet
      |> IO.iodata_to_binary()
      |> Handshake.decode()

    assert {:ok, {@local_peer_id, @info_hash, _}} = handshake
  end

  test "Peer expresses interest & unchokes after we send a bitfield", %{lsock: lsock, session: session} do
    {:ok, sock} = connect(lsock, session)
    {:ok, bitmsg} = bitfield_msg([0])
    :ok = :gen_tcp.send(sock, bitmsg)
    msg1 = recv_message(sock)
    msg2 = recv_message(sock)
    :ok = :gen_tcp.close(sock)

    assert {:ok, :interested} = msg1
    assert {:ok, :unchoke} = msg2
  end

  test "Peer requests blocks when we unchoke them", %{lsock: lsock, session: session} do
    {:ok, sock} = connect(lsock, session)
    {:ok, bitmsg} = bitfield_msg([0])
    :ok = :gen_tcp.send(sock, bitmsg)
    _msg1 = recv_message(sock)
    _msg2 = recv_message(sock)
    :ok = send_message(sock, :unchoke)
    request_msg = recv_message(sock)
    :ok = :gen_tcp.close(sock)

    assert {:ok, {:request, _}} = request_msg
  end

  test "Peer accepts blocks and requests another", %{lsock: lsock, session: session} do
    {:ok, sock} = connect(lsock, session)
    {:ok, bitmsg} = bitfield_msg([0])
    :ok = :gen_tcp.send(sock, bitmsg)
    _interested = recv_message(sock)
    _unchoke = recv_message(sock)
    :ok = send_message(sock, :unchoke)
    _request = recv_message(sock)
    :ok = send_message(sock, {:piece, 0, 0, <<1, 2, 3, 4, 5>>})
    next_request = recv_message(sock)
    :ok = :gen_tcp.close(sock)

    assert {:ok, {:request, _}} = next_request
  end

  defp send_message(sock, msg) do
    case Messages.encode(msg) do
      {:ok, msg_bin} -> :gen_tcp.send(sock, msg_bin)
    end
  end

  defp recv_message(sock) do
    case :gen_tcp.recv(sock, 0, 1_000) do
      {:ok, packet} -> iodata_to_message(packet)
    end
  end

  defp bitfield_msg(pieces) when is_list(pieces) do
    bitfield = pieces
    |> IntSet.new()
    |> IntSet.bitstring()
    |> right_pad(@meta.info.piece_length)
    Messages.encode({:bitfield, bitfield})
  end


  defp connect(lsock, session) do
    {:ok, _pid} = Peer.connect(@remote_address, @local_peer_id, @info_hash, session)
    {:ok, sock} = :gen_tcp.accept(lsock, 1_000)
    {:ok, _handshake} = :gen_tcp.recv(sock, 68)
    :ok = :gen_tcp.send(sock, @remote_handshake)
    :ok = :inet.setopts(sock, packet: 4)
    {:ok, sock}
  end

  defp iodata_to_message(iodata) do
    iodata
    |> IO.iodata_to_binary()
    |> Messages.decode()
  end

  defp right_pad(bin, size_bytes) when is_bitstring(bin) and is_integer(size_bytes) and size_bytes > 0 do
    target_bit_size = size_bytes * 8
    pad_size = target_bit_size - bit_size(bin)
    if pad_size > 0 do
      <<bin::bitstring, 0::size(pad_size)>>
    else
      bin
    end
  end

  test "Peer hands blocks to parent session", %{lsock: lsock, session: session} do
    {:ok, sock} = connect(lsock, session)
    {:ok, bitmsg} = bitfield_msg([0])
    :ok = :gen_tcp.send(sock, bitmsg)
    _interested = recv_message(sock)
    _unchoke = recv_message(sock)
    :ok = send_message(sock, :unchoke)
    _request = recv_message(sock)
    :ok = send_message(sock, {:piece, 0, 0, "t"})
    :ok = :gen_tcp.close(sock)

    # Let the peer pass the piece to the session; this is asynchronous because
    # We are hoping that the peer is reacting to TCP messages
    :timer.sleep(50)

    %{blocks: blocks} = callback_state(session)

    assert blocks == MapSet.new([%{index: 0, offset: 0, data: "t"}])
  end

  defp callback_state(pid) do
    {:status, _, _, [_, _, _, _, [_, _, {:data, [{'State', state}]}]]} = :sys.get_status(pid)
    state
  end
end
