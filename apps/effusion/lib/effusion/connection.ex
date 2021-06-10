defmodule Effusion.Connection do
  @enforce_keys [:direction, :info_hash, :address]
  defstruct [
    direction: nil,
    state: :disconnected,
    info_hash: nil,
    address: nil,
    socket: nil,
    am_choking: true,
    am_interested: false,
    peer_choking: true,
    peer_interested: false
  ]

  def can_download?(%__MODULE__{peer_choking: choking, am_interested: interested}) do
    not choking and interested
  end

  def can_upload?(%__MODULE__{am_choking: choking, peer_interested: interested}) do
    not choking and interested
  end

  def handshake_sent(%__MODULE__{direction: :incoming, state: :handshake_received} = conn) do
    %__MODULE__{conn | state: :connected}
  end

  def handshake_sent(%__MODULE__{direction: :outgoing, state: :disconnected} = conn) do
    %__MODULE__{conn | state: :handshake_sent}
  end

  def handshake_received(%__MODULE__{direction: :incoming, state: :disconnected} = conn) do
    %__MODULE__{conn | state: :received_handshake}
  end

  def handshake_received(%__MODULE__{direction: :outgoing, state: :handshake_sent} = conn) do
    %__MODULE__{conn | state: :connected}
  end

  def unchoke_peer(%__MODULE__{} = conn) do
    %__MODULE__{conn | am_choking: false}
  end

  def choke_peer(%__MODULE__{} = conn) do
    %__MODULE__{conn | am_choking: true}
  end

  def unchoke_us(%__MODULE__{} = conn) do
    %__MODULE__{conn | peer_choking: false}
  end

  def choke_us(%__MODULE__{} = conn) do
    %__MODULE__{conn | peer_choking: true}
  end

  def interested_in_peer(%__MODULE__{} = conn) do
    %__MODULE__{conn | am_interested: true}
  end

  def not_interested_in_peer(%__MODULE__{} = conn) do
    %__MODULE__{conn | am_interested: false}
  end

  def peer_interested_in_us(%__MODULE__{} = conn) do
    %__MODULE__{conn | peer_interested: true}
  end

  def peer_not_interested_in_us(%__MODULE__{} = conn) do
    %__MODULE__{conn | peer_interested: false}
  end

  def set_socket(%__MODULE__{} = conn, socket) do
    %__MODULE__{conn | socket: socket}
  end
end
