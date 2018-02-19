defmodule Effusion.PWP.PeerSocket do
  use GenServer
  alias Effusion.PWP.Messages

  def start_link(opts) do
    parent_process = self()
    GenServer.start_link(__MODULE__, opts)
  end

  def init([parent_process, lsock]) do
    {:ok, %{parent_process: parent_process, lsock: lsock}, 0}
  end

  def handle_info(:timeout, %{parent_process: parent_process, lsock: lsock}) do
    {:ok, sock} = :gen_tcp.accept(lsock)

    {:ok, packet} = :gen_tcp.recv(sock, 0)
    {:ok, msg} = packet |> IO.iodata_to_binary() |> Messages.decode()
    send parent_process, msg
  end
end
