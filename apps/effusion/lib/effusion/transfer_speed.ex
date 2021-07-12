defmodule Effusion.TransferSpeed do
  @moduledoc """
  A struct for recording byte transfers and calculating transfer speeds.
  Add samples with `add_sample/3` and get the current speed with `mean_speed/2`.

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:01Z])
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:02Z])
      ...> |> Effusion.TransferSpeed.mean_speed(~U[2021-07-12 12:00:05Z])
      3.0

  By default, `mean_speed/2` calculates a twenty-second rolling average.
  Samples older than twenty seconds are dropped whenever the struct is updated or queried.

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:25Z])
      []

  To change this period, override the `:period` key when creating this struct.

      iex> %Effusion.TransferSpeed{period: 30}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:25Z])
      [{~U[2021-07-12 12:00:00Z], 20}]

  Functions in this module assume that the given timemstaps are the current time,
  and will raise an error if a timestamp is provided that is earlier than any of the samples.

  For example, the following code will raise an error:

      %Effusion.TransferSpeed{}
      |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:30Z])
      |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:00Z])
  """

  defstruct [
    samples: [],
    period: 20
  ]

  defguardp is_period(period) when is_integer(period) and period > 0

  @doc """
  Returns all samples added in the last `:period` seconds.

  ## Examples

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:10Z])
      [{~U[2021-07-12 12:00:00Z], 20}]

  Expired samples are dropped

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:30Z])
      []
  """
  def samples(%__MODULE__{} = transfer, timestamp) do
    %__MODULE__{samples: samples} = drop_old_samples(transfer, timestamp)
    samples
  end

  @doc """
  Returns the sum of all samples added in the last `:period` seconds.

  ## Examples

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.add_sample(30, ~U[2021-07-12 12:00:01Z])
      ...> |> Effusion.TransferSpeed.transferred(~U[2021-07-12 12:00:10Z])
      50

  Expired samples are dropped

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.add_sample(30, ~U[2021-07-12 12:00:15Z])
      ...> |> Effusion.TransferSpeed.transferred(~U[2021-07-12 12:00:30Z])
      30
  """
  def transferred(%__MODULE__{} = transfer, now) do
    samples(transfer, now)
    |> Enum.map(fn {_timestamp, bytes} ->
      bytes
    end)
    |> Enum.sum()
  end


  @doc """
  Adds a sample to the list of samples we are doing statistics on.

  ## Examples

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:10Z])
      [{~U[2021-07-12 12:00:00Z], 20}]

  Expired samples are dropped.

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.samples(~U[2021-07-12 12:00:30Z])
      []
  """
  def add_sample(%__MODULE__{} = transfer, bytes_count, timestamp) do
    transfer
    |> do_add_sample(bytes_count, timestamp)
    |> drop_old_samples(timestamp)
  end

  @doc """
  Get the mean transfer speed over the last twenty seconds, in bytes per second.

  ## Examples

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:01Z])
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:02Z])
      ...> |> Effusion.TransferSpeed.mean_speed(~U[2021-07-12 12:00:05Z])
      3.0

  Expired samples are dropped.

      iex> %Effusion.TransferSpeed{}
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:00Z])
      ...> |> Effusion.TransferSpeed.add_sample(20, ~U[2021-07-12 12:00:20Z])
      ...> |> Effusion.TransferSpeed.mean_speed(~U[2021-07-12 12:00:30Z])
      1.0
  """
  def mean_speed(%__MODULE__{period: period} = transfer, now) when is_period(period) do
    transferred(transfer, now) / period
  end

  defp do_add_sample(%__MODULE__{samples: samples} = transfer, bytes_count, timestamp) do
    updated_samples = [{timestamp, bytes_count} | samples]

    %__MODULE__{transfer | samples: updated_samples}
  end

  defp drop_old_samples(%__MODULE__{samples: samples, period: period} = transfer, now) when is_period(period) do
    updated_samples =
      Enum.filter(samples, fn {timestamp, _bytes} ->
        age = DateTime.diff(now, timestamp, :second)

        if age < 0 do
          raise "Effusion.TransferSpeed tried to drop samples that were in the future. A sample with timestamp #{timestamp} was found but the current time was given as #{now}."
        end

        age <= period
      end)

    %__MODULE__{transfer | samples: updated_samples}
  end
end
