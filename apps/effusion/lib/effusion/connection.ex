defmodule Effusion.Connection do
  @moduledoc """
  A struct containing information about a connection to a peer.
  """

  @enforce_keys [:direction, :info_hash, :address]
  defstruct [
    direction: nil,
    info_hash: nil,
    address: nil,
    socket: nil,
    am_choking: true,
    am_interested: false,
    peer_choking: true,
    peer_interested: false
  ]

  @doc """
  Returns a boolean indicating if we can download bytes from a peer.

  ## Examples

      iex> %Effusion.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connection.can_download?()
      false

      iex> %Effusion.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connection.unchoke_us()
      ...> |> Effusion.Connection.interested_in_peer()
      ...> |> Effusion.Connection.can_download?()
      true
  """
  def can_download?(%__MODULE__{peer_choking: choking, am_interested: interested}) do
    not choking and interested
  end

  @doc """
  Returns a boolean indicating if we can upload bytes to a peer.

  ## Examples

      iex> %Effusion.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connection.can_upload?()
      false

      iex> %Effusion.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connection.unchoke_peer()
      ...> |> Effusion.Connection.peer_interested_in_us()
      ...> |> Effusion.Connection.can_upload?()
      true
  """
  def can_upload?(%__MODULE__{am_choking: choking, peer_interested: interested}) do
    not choking and interested
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
