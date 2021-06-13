defmodule Effusion.ConnectionWorker do
  alias Effusion.TCPWorker

  def send(info_hash, address, message) do
    TCPWorker.send(info_hash, address, message)
  end

  def broadcast(info_hash, message) do
    TCPWorker.broadcast(info_hash, message)
  end
end
