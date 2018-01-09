defmodule Effusion.IntSet do
  use Bitwise
  @moduledoc """
  Efficiently store and index a set of non-negative integers.
  """

  @doc """
  Create an empty int set.

  ## Examples

      iex> Effusion.IntSet.new
      0

  """
  def new do
    0
  end

  @doc """
  Create an int set from a bitfield

  ## Examples

      iex> Effusion.IntSet.new(<<64, 0>>)
      16384

      iex> Effusion.IntSet.new(<<1>>)
      1

      iex> Effusion.IntSet.new(<<0>>)
      0

  """
  def new(bitfield) when is_binary(bitfield) do
    i = bit_size(bitfield)
    <<a :: size(i)>> = bitfield
    a
  end

  @doc """
  Create an int set containing a list of integers

  ## Examples

      iex> Effusion.IntSet.new([0])
      1

      iex> Effusion.IntSet.new([1])
      2

      iex> Effusion.IntSet.new([0, 1])
      3

  """
  def new(list) when is_list(list) do
    Enum.reduce(list, 0, &(put(&2, &1)))
  end

  @doc """
  Create a new int set containing the given element.

  ## Examples

      iex> Effusion.IntSet.singleton(0)
      1


      iex> Effusion.IntSet.singleton(8)
      256
  """
  def singleton(member) when member >= 0 do
    1 <<< member
  end

  @doc """
  Create a new set that contains all of the elements of both x and y.

  ## Examples

      iex> Effusion.IntSet.union(8, 128)
      136

  """
  def union(x, y) when x >= 0 and y >= 0 do
    x ||| y
  end

  @doc """
  Test if the given int set contains a value

  ## Examples

      iex> Effusion.IntSet.member?(128, 7)
      true

      iex> Effusion.IntSet.member?(128, 6)
      false

  """
  def member?(s, x) when s >= 0 and x >= 0 do
    i = (1 <<< x)
    (i &&& s) != 0
  end

  @doc """
  Add a value to the int set

  ## Examples

      iex> Effusion.IntSet.put(0, 7)
      128

      iex> Effusion.IntSet.put(128, 3)
      136

  """
  def put(s, x) when s >= 0 and x >= 0 do
    i = (1 <<< x)
    i ||| s
  end
end
