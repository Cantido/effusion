defmodule Effusion.PWP.TCP.Socket do
  require Logger
  alias Effusion.PWP.Messages
  alias Effusion.PWP.Messages.Handshake
  alias Effusion.Statistics.Net, as: NetStats

  @moduledoc """
  Interface to Peer Wire Protocol (PWP) sockets.
  """

  @doc """
  Set up a socket to listen on `port` on the local host.
  """
  def listen(port) do
    :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true, send_timeout: 1_000])
  end

  @doc """
  Connect to a server described by `peer`.
  """
  def connect({host, port}, local_info_hash, local_peer_id, expected_peer_id) do
    case :gen_tcp.connect(host, port, [:binary, active: false], 10_000) do
      {:ok, socket} -> Handshake.perform(socket, local_peer_id, expected_peer_id, local_info_hash)
      err -> err
    end
  end

  @doc """
  Accepts an incoming connection a listening socket,
  and performs a PWP handshake as the given `peer`.
  """
  def accept(lsock, local_info_hash, local_peer_id, expected_peer_id) do
    case :gen_tcp.accept(lsock, 1_000) do
      {:ok, socket} -> Handshake.perform(socket, local_peer_id, expected_peer_id, local_info_hash)
      err -> err
    end
  end

  @doc """
  Send a message on a socket.
  """
  def send_msg(socket, msg) do
    case Messages.encode(msg) do
      {:ok, request} ->
        Messages.payload_bytes_count(request) |> NetStats.add_sent_payload_bytes()
        byte_size(request) |> NetStats.add_sent_bytes()
        :gen_tcp.send(socket, request)

      err ->
        err
    end
  end

  @doc """
  Receives a message from a socket in passive mode.
  """
  def recv(socket, size \\ 0) do
    case :gen_tcp.recv(socket, size) do
      {:ok, packet} ->
        Messages.payload_bytes_count(packet) |> NetStats.add_recv_payload_bytes()
        byte_size(packet) |> NetStats.add_recv_bytes()
        decode(packet)

      err ->
        err
    end
  end

  @doc """
  Decode a binary message.
  """
  def decode(data) do
    Messages.decode(data)
  end

  @doc """
  Closes a PWP socket.
  """
  def close(socket) do
    :gen_tcp.close(socket)
  end

  @doc """
  Get the address that the PWP socket is connected to.
  """
  def address(socket) when not is_nil(socket) do
    :inet.peername(socket)
  end
end