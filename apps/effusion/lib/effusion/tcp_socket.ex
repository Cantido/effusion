defmodule Effusion.TCPSocket do
  require Logger
  alias Effusion.Messages

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
    case :gen_tcp.connect(host, port, [:binary, active: false, keepalive: true], 10_000) do
      {:ok, socket} ->
        perform_handshake(
          socket,
          local_peer_id,
          expected_peer_id,
          local_info_hash,
          our_extensions
        )

      err ->
        err
    end
  end

  @doc """
  Accepts an incoming connection a listening socket,
  and performs a PWP handshake as the given `peer`.
  """
  def accept(lsock, local_info_hash, local_peer_id, expected_peer_id, our_extensions \\ []) do
    case :gen_tcp.accept(lsock, 5_000) do
      {:ok, socket} ->
        perform_handshake(
          socket,
          local_peer_id,
          expected_peer_id,
          local_info_hash,
          our_extensions
        )

      err ->
        err
    end
  end

  defp perform_handshake(socket, local_peer_id, expected_peer_id, local_info_hash, our_extensions) do
    with {:ok, _bytes_count} <- send_msg(socket, {:handshake, local_peer_id, local_info_hash, our_extensions}),
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
  Send a message on a socket.

  Returns `{:ok, bytes_count}` on success, or the relevant error.
  """
  def send_msg(socket, msg) do
    with {:ok, request} <- Messages.encode(msg),
         :ok <- :gen_tcp.send(socket, request) do
      {:ok, byte_size(request)}
    end
  end

  @doc """
  Receives a message from a socket in passive mode.
  """
  def recv(socket, size \\ 0) do
    with {:ok, packet} <- :gen_tcp.recv(socket, size) do
      decode(packet)
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
