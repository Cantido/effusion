defmodule Effusion.Connections.Connection do
  @moduledoc """
  A struct containing information about a connection to a peer.
  """

  alias Effusion.TransferSpeed

  @enforce_keys [:direction, :info_hash, :address]
  defstruct [
    direction: nil,
    info_hash: nil,
    address: nil,
    socket: nil,
    am_choking: true,
    am_interested: false,
    peer_choking: true,
    peer_interested: false,
    download_speed: %TransferSpeed{},
    upload_speed: %TransferSpeed{},
  ]

  @doc """
  Returns a boolean indicating if we can download bytes from a peer.

  ## Examples

      iex> %Effusion.Connections.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connections.Connection.can_download?()
      false

      iex> %Effusion.Connections.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connections.Connection.unchoke_us()
      ...> |> Effusion.Connections.Connection.interested_in_peer()
      ...> |> Effusion.Connections.Connection.can_download?()
      true
  """
  def can_download?(%__MODULE__{peer_choking: choking, am_interested: interested}) do
    not choking and interested
  end

  @doc """
  Returns a boolean indicating if we can upload bytes to a peer.

  ## Examples

      iex> %Effusion.Connections.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connections.Connection.can_upload?()
      false

      iex> %Effusion.Connections.Connection{direction: :outgoing, info_hash: "1234567890", address: {{127, 0, 0, 1}, 8001}}
      ...> |> Effusion.Connections.Connection.unchoke_peer()
      ...> |> Effusion.Connections.Connection.peer_interested_in_us()
      ...> |> Effusion.Connections.Connection.can_upload?()
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

  def add_download_event(%__MODULE__{download_speed: dl} = conn, bytes_count, timestamp) do
    updated_dl = TransferSpeed.add_sample(dl, bytes_count, timestamp)
    %__MODULE__{conn | download_speed: updated_dl}
  end

  def download_speed(%__MODULE__{download_speed: dl}, now) do
    TransferSpeed.mean_speed(dl, now)
  end

  def add_upload_event(%__MODULE__{upload_speed: ul} = conn, bytes_count, timestamp) do
    updated_ul = TransferSpeed.add_sample(ul, bytes_count, timestamp)
    %__MODULE__{conn | upload_speed: updated_ul}
  end

  def upload_speed(%__MODULE__{upload_speed: ul}, now) do
    TransferSpeed.mean_speed(ul, now)
  end
end
