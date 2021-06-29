defmodule Effusion.Connection do
  @enforce_keys [:direction, :info_hash, :address]
  defstruct [
    direction: nil,
    info_hash: nil,
    address: nil,
    socket: nil,
    upload_events: [],
    download_events: [],
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

  def download_speed(%__MODULE__{} = conn) do
    Enum.map(conn.download_events, &elem(&1, 0))
    |> mean()
  end

  def add_download_event(%__MODULE__{} = conn, byte_count, now) do
    dl_events =
      conn.download_events
      |> Enum.reject(fn {_byte_count, ts} ->
        DateTime.diff(ts, now) > 20
      end)

    dl_events = [{byte_count, now} | dl_events]

    %__MODULE__{conn | download_events: dl_events}
  end

  def upload_speed(%__MODULE__{} = conn) do
    Enum.map(conn.upload_events, &elem(&1, 0))
    |> mean()
  end

  def add_upload_event(%__MODULE__{} = conn, byte_count, now) do
    ul_events =
      conn.upload_events
      |> Enum.reject(fn {_byte_count, ts} ->
        DateTime.diff(ts, now) > 20
      end)

    ul_events = [{byte_count, now} | ul_events]

    %__MODULE__{conn | upload_events: ul_events}
  end

  defp mean(samples) do
    Enum.sum(samples) / Enum.count(samples)
  end
end
