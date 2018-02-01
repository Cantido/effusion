defmodule Effusion.IntSet do
  use Bitwise
  alias Effusion.IntSet
  @moduledoc """
  Efficiently store and index a set of non-negative integers.

  Binary strings are interpreted as little-endian, which means that the most-sigificant bit
  corresponds to the integer 0.
  """

  defstruct s: <<>>

  defguard is_index(i)
    when is_integer(i)
     and i >= 0

  @doc """
  Create an empty int set.

  ## Examples

      iex> Effusion.IntSet.new
      %Effusion.IntSet{s: <<>>}

  """
  def new do
    %IntSet{}
  end

  @doc """
  Create an int set from a bitfield.

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
    %IntSet{s: bitfield}
  end

  def new(list) when is_list(list) do
    Enum.reduce(list, new(), &(put(&2, &1)))
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
  def singleton(member) when is_index(member) do
    new([member])
  end

  @doc """
  Create a new set that contains all of the elements of both x and y.

  ## Examples

      iex> a = Effusion.IntSet.new([7])
      iex> b = Effusion.IntSet.new([4])
      iex> u = Effusion.IntSet.union(a, b)
      iex> Effusion.IntSet.member?(u, 7)
      true
      iex> Effusion.IntSet.member?(u, 4)
      true

  """
  def union(x, y)

  def union(
    %IntSet{s: <<a :: 1, arest :: bitstring>>},
    %IntSet{s: <<b :: 1, brest :: bitstring>>}
  ) do
    %IntSet{s: tail_bin} = union new(arest), new(brest)
    %IntSet{s: <<(a ||| b) :: 1, tail_bin :: bitstring>>}
  end

  def union(%IntSet{} = a, %IntSet{s: <<>>}), do: a
  def union(%IntSet{s: <<>>}, %IntSet{} = b), do: b
  def union(%IntSet{s: <<>>}, %IntSet{s: <<>>}), do: %IntSet{}

  @doc """
  Test if the given int set contains a value

  # ## Examples

      iex> set = Effusion.IntSet.new([7])
      iex> Effusion.IntSet.member?(set, 7)
      true
      iex> Effusion.IntSet.member?(set, 6)
      false

  """
  def member?(s, x)

  def member?(%IntSet{s: s}, x) when is_index(x) and bit_size(s) <= x, do: false
  def member?(%IntSet{s: <<0 :: 1, _rst :: bitstring>>}, 0), do: false
  def member?(%IntSet{s: <<1 :: 1, _rst :: bitstring>>}, 0), do: true

  def member?(%IntSet{s: s}, x)
  when is_index(x)
   and bit_size(s) > x
  do
    <<_ :: size(x), i :: 1, _ :: bitstring>> = s
    i == 1
  end

  @doc """
  Add a value to the int set

  ## Examples

      iex> set = Effusion.IntSet.new()
      iex> set = Effusion.IntSet.put(set, 0)
      iex> Effusion.IntSet.member?(set, 0)
      true

  """
  def put(s, x)

  def put(%IntSet{s: <<>>}, x) when is_index(x) do
    %IntSet{s: <<0 :: size(x), 1 :: 1>>}
  end

  def put(%IntSet{s: s}, x) when is_index(x) and is_bitstring(s) and bit_size(s) > x do
    <<pre :: size(x), _ :: 1, post :: bitstring>> = s
    %IntSet{s: <<pre :: size(x), 1 :: 1, post :: bitstring>>}
  end

  def put(%IntSet{s: s}, x) when is_index(x) and is_bitstring(s) and bit_size(s) <= x do
    pre_size = bit_size(s)
    needed_bits = x - pre_size

    %IntSet{s: <<s :: bitstring, 0 :: size(needed_bits), 1 :: 1>>}
  end

  defimpl Collectable do
    def into(original) do
      collector_fun = fn
        set, {:cont, elem} -> IntSet.put(set, elem)
        set, :done -> set
        _set, :halt -> :ok
      end

      {original, collector_fun}
    end
  end

  defimpl Enumerable do
    def count(s) do
      {:error, __MODULE__}
    end

    def member?(s, e) do
      {:error, __MODULE__}
    end

    def slice(s) do
      {:error, __MODULE__}
    end

    def reduce(_, {:halt, acc}, _fun) do
      {:halted, acc}
    end

    def reduce(set, {:suspend, acc}, fun) do
      {:suspended, acc, &reduce(set, &1, fun)}
    end

    def reduce(%IntSet{s: <<>>}, {:cont, acc}, _fun) do
      {:done, acc}
    end

    def reduce(%IntSet{s: s}, {:cont, acc}, fun) do
      last_i = bit_size(s) - 1
      before_last_size = last_i
      <<h :: bitstring-size(before_last_size), last_flag :: 1>> = s

      rest = IntSet.new(h)

      if last_flag == 1 do
        reduce(rest, fun.(last_i, acc), fun)
      else
        reduce(rest, {:cont, acc}, fun)
      end
    end
  end
end
