defmodule Effusion.Transport do
  @opaque socket :: any | :inet.socket

  @type address :: :inet.socket_address | :inet.hostname

  @callback connect(address, :inet.port_number, [any]) :: {:ok, socket} | {:error, any}
  @callback send(socket, binary) :: :ok | {:error, any()}
  @callback recv(socket, non_neg_integer) :: {:ok, String.t | binary} | {:error, any()}
end
