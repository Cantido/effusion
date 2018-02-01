defmodule Effusion.IntSet do
  use Bitwise
  @moduledoc """
  Efficiently store and index a set of non-negative integers.
  """

  @doc """
  Create an empty int set.

  ## Examples

      iex> Effusion.IntSet.new
      <<>>

  """
  def new do
    <<>>
  end

  @doc """
  Create an int set from a bitfield. Binary strings are interpreted as
  little-endian, which means that the most-sigificant bit corresponds
  to the integer 0.

  ## Examples

      iex> set = Effusion.IntSet.new(<<1 :: 1>>)
      iex> Effusion.IntSet.member?(set, 0)
      true

      iex> set = Effusion.IntSet.new(<<0b1000_1000>>)
      iex> Effusion.IntSet.member?(set, 0)
      true
      iex> Effusion.IntSet.member?(set, 1)
      false
      iex> Effusion.IntSet.member?(set, 4)
      true

      iex> set = Effusion.IntSet.new(<<0 :: 1>>)
      iex> Effusion.IntSet.member?(set, 0)
      false

  You can also provide a list of integers.

      iex> set = Effusion.IntSet.new([0])
      iex> Effusion.IntSet.member?(set, 0)
      true

  """
  def new(bitfield)

  def new(bitfield) when is_bitstring(bitfield) do
    bitfield
  end

  def new(list) when is_list(list) do
    Enum.reduce(list, new, &(put(&2, &1)))
  end

  @doc """
  Create a new int set containing the given element.

  ## Examples

      iex> set = Effusion.IntSet.singleton(0)
      iex> Effusion.IntSet.member?(set, 0)
      true

      iex> set = Effusion.IntSet.singleton(8)
      iex> Effusion.IntSet.member?(set, 8)
      true
  """
  def singleton(member) when member >= 0 do
    new([member])
  end

  @doc """
  Create a new set that contains all of the elements of both x and y.

  ## Examples

      iex> Effusion.IntSet.union(<<0 :: 1>>, <<0 :: 1>>)
      <<0 :: 1>>

      iex> Effusion.IntSet.union(<<0b0000_0000>>, <<0b0000_0000>>)
      <<0b0000_0000>>

  """
  def union(x, y)

  def union(<<a :: 1, arest :: bitstring>>, <<b :: 1, brest :: bitstring>>) do
    <<(a ||| b) :: 1, union(arest, brest) :: bitstring>>
  end

  def union(a, <<>>), do: a
  def union(<<>>, b), do: b
  def union(<<>>, <<>>), do: <<>>

  @doc """
  Test if the given int set contains a value

  ## Examples

      iex> Effusion.IntSet.member?(<<0b0000_0001>>, 7)
      true

      iex> Effusion.IntSet.member?(<<0b0000_0101>>, 5)
      true

      iex> Effusion.IntSet.member?(<<0b0000_0001>>, 6)
      false

  """
  def member?(s, x)

  def member?(<<>>, x), do: false
  def member?(<<0 :: 1, _rst :: bitstring>>, 0), do: false
  def member?(<<1 :: 1, _rst :: bitstring>>, 0), do: true
  def member?(<<_ :: 1, rest :: bitstring>>, x), do: member?(rest, x - 1)

  @doc """
  Add a value to the int set

  ## Examples

      iex> Effusion.IntSet.put(<<>>, 7)
      <<0b0000_0001>>

      iex> Effusion.IntSet.put(<<0b0000_0001>>, 3)
      <<0b0001_0001>>

  """
  def put(s, x) do
    nil
  end

end
