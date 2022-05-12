defmodule Effusion.ConnectionWorker do
  alias Effusion.Connections

  def send(info_hash, address, message) do
    Connections.send(info_hash, address, message)
  end

  def broadcast(info_hash, message) do
    Connections.broadcast(info_hash, message)
  end
end
