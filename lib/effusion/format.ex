defmodule Effusion.Format do
  import Effusion.Hash

  @kibibyte 1024
  @mebibyte 1024 * @kibibyte
  @gibibyte 1024 * @mebibyte
  @tebibyte 1024 * @gibibyte

  def bytes(n) when n >= 0 do
    cond do
      n < @kibibyte -> "#{n} B"
      n < @mebibyte -> "#{Float.round(n/@kibibyte, 1)} kiB"
      n < @gibibyte -> "#{Float.round(n/@mebibyte, 1)} MiB"
      n < @tebibyte -> "#{Float.round(n/@gibibyte, 1)} GiB"
      true -> "#{Float.round(n/@tebibyte, 1)} TiB"
    end
  end

  def info_hash(hash) when is_hash(hash) do
    Effusion.Hash.inspect(hash)
  end

  @doc """
  Creates a string progress bar with a progressively shaded fractional portion

  ## Examples

      iex> Effusion.Format.progress_bar(0.0, 20)
      "├──────────────────┤"

      iex> Effusion.Format.progress_bar(50.0, 20)
      "██████████─────────┤"

      iex> Effusion.Format.progress_bar(33.0, 10)
      "███░─────┤"

      iex> Effusion.Format.progress_bar(36.0, 10)
      "███▒─────┤"

      iex> Effusion.Format.progress_bar(38.0, 10)
      "███▓─────┤"

  """
  def progress_bar(percent, width) when is_float(percent) and is_integer(width) and percent >= 0 and width > 0 do
    # example: 33% of 80 characters
    fraction = (percent / 100) # 33.0 / 100 = 0.33
    characters_completed_float = fraction * width # 0.33 * 80 = 26.4
    characters_completed = trunc(characters_completed_float) # trunc(26.4) = 26

    # Represent fractional part with a partially shaded box.
    character_remainder = characters_completed_float - trunc(characters_completed_float) # 26.4 - 26 = 0.4
    intermediate_character = cond do
      character_remainder >= 0.75 -> "▓"
      character_remainder >= 0.5 -> "▒" #
      character_remainder >= 0.25 -> "░"
      characters_completed == 0 -> "├"
      true -> "─"
    end

    characters_empty = width - characters_completed - 2

    String.duplicate("█", characters_completed) <>
      intermediate_character <>
      String.duplicate("─", characters_empty) <> "┤"
  end
end
