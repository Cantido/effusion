defmodule Effusion.PWP.Peer do
  @behaviour Effusion.PWP

  def connect(_host, _port, _peer_id, _info_hash) do
    :error
  end

  def recv(_socket) do
    :error
  end
end
