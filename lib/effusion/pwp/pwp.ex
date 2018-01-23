defmodule Effusion.PWP do
  @callback connect(
    host:: :inet.hostname() | :inet.ip_address(),
    port:: :inet.port_number(),
    peer_id :: Effusion.peer_id(),
    info_hash :: Effusion.info_hash()
  ) :: {:ok, pid()} | :error

  @callback recv(pid) :: {:ok, message} | :error

  @type message :: atom
end
