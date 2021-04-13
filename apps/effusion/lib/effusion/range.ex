defmodule Effusion.Range do
  @moduledoc """
  Functions for working with `Range`s.


  Remember that ranges are inclusive, and `0..0` would have a length of `1`.
  This module uses `nil` to indicate a non-existent or invalid range.
  """

  @doc """
  Creates a `Range` from a starting value and a length.

  ## Examples

      iex> Effusion.Range.poslen(5, 5)
      5..9

      iex> Effusion.Range.poslen(0, 1)
      0..0
  """
  def poslen(start, length) when length > 0 do
    start..(start + length - 1)
  end

  @doc """
  Shift a range in one direction or another.

  ## Examples

      iex> Effusion.Range.shift(0..1, 3)
      3..4

      iex> Effusion.Range.shift(5..10, -5)
      0..5
  """
  def shift(first..last, distance) do
    (first + distance)..(last + distance)
  end

  @doc """
  Returns `true` if the ranges `a` and `b` overlap; `false` otherwise.

  ## Examples

      iex> Effusion.Range.overlap?(0..5, 5..10)
      true

      iex> Effusion.Range.overlap?(0..1, 2..3)
      false
  """
  def overlap?(a, b) do
    overlap_len(a, b) > 0
  end

  @doc """
  Returns the position within b, and length, of the overlapping section.

  ## Examples

      iex> Effusion.Range.overlap_poslen(0..10, 2..3)
      {0, 2}

      iex> Effusion.Range.overlap_poslen(4..5, 0..10)
      {4, 2}

      iex> Effusion.Range.overlap_poslen(0..1, 2..3)
      {0, 0}
  """
  def overlap_poslen(a_start..a_end, b_start..b_end) do
    pos_in_piece = max(0, a_start - b_start)
    overlap_len = overlap_len(a_start..a_end, b_start..b_end)
    {pos_in_piece, overlap_len}
  end

  @doc """
  Find the length of the overlap between two ranges.

  ## Examples

      iex> Effusion.Range.overlap_len(0..5, 2..7)
      4

      iex> Effusion.Range.overlap_len(0..5, 5..7)
      1

      iex> Effusion.Range.overlap_len(0..1, 2..3)
      0
  """
  def overlap_len(a, b) do
    case overlap(a, b) do
      nil -> 0
      l -> Enum.count(l)
    end
  end

  @doc """
  Find the range that is the overlap between two ranges.

  ## Examples

      iex> Effusion.Range.overlap(0..5, 2..7)
      2..5

      iex> Effusion.Range.overlap(0..5, 5..10)
      5..5

      iex> Effusion.Range.overlap(0..3, 6..9)
      nil
  """
  def overlap(a_start..a_end, b_start..b_end) when a_end >= b_start and a_start <= b_end do
    overlap_start = max(b_start, a_start)
    overlap_end = min(b_end, a_end)
    overlap_start..overlap_end
  end

  def overlap(_, _), do: nil
end
