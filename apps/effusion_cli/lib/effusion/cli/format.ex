defmodule Effusion.CLI.Format do
  import Effusion.Hash

  @moduledoc """
  Functions for presenting values as strings.
  """

  @kibibyte 1024
  @mebibyte 1024 * @kibibyte
  @gibibyte 1024 * @mebibyte
  @tebibyte 1024 * @gibibyte

  @doc """
  Rounds an integer number of bytes into a higher unit.

  ## Examples

      iex> Effusion.CLI.Format.bytes(109)
      "109 B"

      iex> Effusion.CLI.Format.bytes(108_462)
      "105.9 kiB"

      iex> Effusion.CLI.Format.bytes(64_700_000)
      "61.7 MiB"

      iex> Effusion.CLI.Format.bytes(150_000_000_000)
      "139.7 GiB"

      iex> Effusion.CLI.Format.bytes(426_000_000_000_000)
      "387.4 TiB"
  """
  def bytes(n) when n >= 0 do
    cond do
      n < @kibibyte -> "#{n} B"
      n < @mebibyte -> "#{Float.round(n / @kibibyte, 1)} kiB"
      n < @gibibyte -> "#{Float.round(n / @mebibyte, 1)} MiB"
      n < @tebibyte -> "#{Float.round(n / @gibibyte, 1)} GiB"
      true -> "#{Float.round(n / @tebibyte, 1)} TiB"
    end
  end

  def info_hash(hash) when is_hash(hash) do
    Effusion.Hash.encode(hash)
  end

  defguardp is_percentage(percent) when is_float(percent) and percent >= 0.0 and percent <= 100.0
  defguardp is_width(width) when is_integer(width) and width > 0

  @doc """
  Creates a string progress bar with a progressively shaded fractional portion

  ## Examples

      iex> Effusion.CLI.Format.progress_bar(0.0, 20)
      "├──────────────────┤"

      iex> Effusion.CLI.Format.progress_bar(50.0, 20)
      "██████████─────────┤"

      iex> Effusion.CLI.Format.progress_bar(33.0, 10)
      "███░─────┤"

      iex> Effusion.CLI.Format.progress_bar(36.0, 10)
      "███▒─────┤"

      iex> Effusion.CLI.Format.progress_bar(38.0, 10)
      "███▓─────┤"

      iex> Effusion.CLI.Format.progress_bar(90.0, 10)
      "█████████┤"

      iex> Effusion.CLI.Format.progress_bar(96.0, 10)
      "█████████▒"

      iex> Effusion.CLI.Format.progress_bar(100.0, 10)
      "██████████"

  """
  def progress_bar(percent, width) when is_percentage(percent) and is_width(width) do
    fraction = percent / 100
    characters_shaded_float = fraction * width

    characters_solid = trunc(characters_shaded_float)
    shade_amount = characters_shaded_float - characters_solid
    characters_partial = ceil(shade_amount)
    characters_filled = characters_solid + characters_partial
    characters_empty = width - characters_filled

    cond do
      characters_filled == 0 ->
        "├" <> String.duplicate("─", width - 2) <> "┤"

      characters_solid == 0 ->
        fractional_shaded_box(shade_amount, "├") <>
          String.duplicate("─", width - 2) <> "┤"

      characters_filled < width && shade_amount > 0 ->
        String.duplicate("█", characters_solid) <>
          fractional_shaded_box(shade_amount, "─") <>
          String.duplicate("─", characters_empty - 1) <> "┤"

      characters_filled < width ->
        String.duplicate("█", characters_solid) <>
          String.duplicate("─", characters_empty - 1) <> "┤"

      characters_solid < width ->
        String.duplicate("█", characters_solid) <>
          fractional_shaded_box(shade_amount, "┤")

      true ->
        String.duplicate("█", width)
    end
  end

  defp fractional_shaded_box(fraction, empty_char) when fraction >= 0 and fraction <= 1 do
    cond do
      fraction >= 1 -> "█"
      fraction >= 0.75 -> "▓"
      fraction >= 0.5 -> "▒"
      fraction >= 0.25 -> "░"
      true -> empty_char
    end
  end
end
