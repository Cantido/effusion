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
  def connect({host, port}, local_info_hash, local_peer_id, expected_peer_id, our_extensions) do
    with {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false], 10_000),
         :ok <- send_msg(socket, {:handshake, local_peer_id, local_info_hash, our_extensions}),
         {:ok, hs = {:handshake, remote_peer_id, _, their_extensions}} <- recv(socket, 68),
         :ok <- validate(expected_peer_id, local_info_hash, hs),
         :ok <- :inet.setopts(socket, packet: 4) do
      {:ok, socket, remote_peer_id, their_extensions}
    else
      err -> err
    end
  end

  defp validate(
         expected_peer_id,
         local_info_hash,
         {:handshake, remote_peer_id, remote_info_hash, _reserved}
       ) do
    cond do
      local_info_hash != remote_info_hash ->
        {:error, {:mismatched_info_hash, [expected: local_info_hash, actual: remote_info_hash]}}

      expected_peer_id != nil and expected_peer_id != remote_peer_id ->
        {:error, {:mismatched_peer_id, [expected: expected_peer_id, actual: remote_peer_id]}}

      true ->
        :ok
    end
  end

  @doc """
  Accepts an incoming connection a listening socket,
  and performs a PWP handshake as the given `peer`.
  """
  def accept(lsock, local_peer_id, our_extensions) do
    with {:ok, socket} <- :gen_tcp.accept(lsock, [:binary, active: false], 1_000),
         {:ok, hs = {:handshake, _, _, their_extensions}} <- recv(socket, 68),
         :ok <- send_msg(socket, {:handshake, local_peer_id, local_info_hash, our_extensions}),
         :ok <- validate(expected_peer_id, local_info_hash, hs),
         :ok <- :inet.setopts(socket, packet: 4) do
    else
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
