defmodule Effusion.PWP.Peer do
  use GenServer, restart: :temporary
  alias Effusion.PWP.Messages.Handshake
  require Logger

  @transport Application.get_env(:effusion, :peer_transport)

  ## API

  def connect({host, port}, peer_id, info_hash) do
    args = [host, port, peer_id, info_hash]
    Effusion.PWP.ConnectionSupervisor.start_child(args)
  end

  def start_link([host, port, peer_id, info_hash]) do
    GenServer.start_link(__MODULE__, [host, port, peer_id, info_hash])
  end

  ## Callbacks

  def init([host, port, peer_id, info_hash]) do
    {
      :ok,
      %{
        host: host,
        port: port,
        peer_id: peer_id,
        info_hash: info_hash
      },
      0
    }
  end

  def handle_info(:timeout, state) do
    state
     |> connect()
     |> handshake()
     |> recv_msg()
  end

  defp connect(state) do
    Logger.info("Starting peer connection to #{inspect(state.host)}:#{state.port} for torrent #{Base.encode16(state.info_hash, case: :lower)}")

    case @transport.connect(state.host, state.port, []) do
      {:ok, socket} -> {:noreply, Map.put(state, :socket, socket)}
      {:error, err} ->
        Logger.info("Could not connect to #{inspect(state.host)}:#{state.port}. Reason: #{inspect(err)}. Abandoning peer.")
        {:stop, :normal, state}
    end
  end

  defp handshake({:noreply, state}) do
    with :ok <- @transport.send(state.socket, Handshake.encode(state.peer_id, state.info_hash)),
         {:ok, handshake} <- @transport.recv(state.socket, 68),
         {:ok, _} <- Effusion.PWP.Messages.Handshake.decode(IO.iodata_to_binary(handshake))
    do
      {:noreply, state}
    else
      {:error, :malformed_handshake} ->
        Logger.info("Peer at #{inspect(state.host)}:#{state.port} failed to handshake. Abandonding peer.")
        {:stop, :normal, state}
      {:error, err} ->
        Logger.info("Handshake with peer at #{inspect(state.host)}:#{state.port} failed to handshake. Reason: #{inspect(err)} Abandonding peer.")
        {:stop, :normal, state}
    end
  end

  defp handshake(err), do: err

  defp recv_msg({:noreply, state}) do
    case @transport.recv(state.socket, 0) do
      {:ok, _} -> {:noreply, state}
      {:error, err} ->
        Logger.info("Receiving message from #{inspect(state.host)}:#{state.port} failed. Reason: #{inspect(err)}. Abandonding peer.")
        {:stop, :normal, state}
    end
  end

  defp recv_msg(err), do: err
end
