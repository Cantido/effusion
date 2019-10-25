defmodule Effusion.PWP.Socket do
  require Logger
  alias Effusion.PWP.Messages

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
  def connect(address, local_info_hash, local_peer_id, expected_peer_id) do
    with {host, port} = address,
         {:ok, socket} <- :gen_tcp.connect(host, port, [:binary, active: false], 10_000) do
      handshake(socket, local_peer_id, expected_peer_id, local_info_hash)
    else
      err -> err
    end
  end

  @doc """
  Accepts an incoming connection a listening socket,
  and performs a PWP handshake as the given `peer`.
  """
  def accept(lsock, local_info_hash, local_peer_id, expected_peer_id) do
    with {:ok, socket} <- :gen_tcp.accept(lsock, 1_000) do
      handshake(socket, local_peer_id, expected_peer_id,local_info_hash)
    else
      err -> err
    end
  end

  defp handshake(socket, local_peer_id, expected_peer_id, local_info_hash) do
    with :ok <- send_msg(socket, {:handshake, local_peer_id, local_info_hash}),
         {:ok, hs = {:handshake, remote_peer_id, _, _}} <- recv(socket, 68),
         :ok <- validate_handshake(expected_peer_id, local_info_hash, hs),
         :ok <- :inet.setopts(socket, packet: 4) do
      {:ok, socket, remote_peer_id}
    else
      err -> err
    end
  end

  def validate_handshake(expected_peer_id, local_info_hash, {:handshake, remote_peer_id, remote_info_hash, _reserved}) do
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
  """
  def send_msg(socket, msg) do
    case Messages.encode(msg) do
      {:ok, request} -> :gen_tcp.send(socket, request)
      err -> err
    end
  end

  @doc """
  Receives a message from a socket in passive mode.
  """
  def recv(socket, size \\ 0) do
    case :gen_tcp.recv(socket, size) do
      {:ok, packet} -> decode(packet)
      err -> err
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
