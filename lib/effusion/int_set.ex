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

  defguard can_contain(s, i)
    when is_index(i)
     and is_bitstring(s)
     and bit_size(s) > i

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
      iex> Enum.member?(set, 0)
      true

      iex> set = Effusion.IntSet.new(<<0b1000_1000>>)
      iex> Enum.member?(set, 0)
      true
      iex> Enum.member?(set, 1)
      false
      iex> Enum.member?(set, 4)
      true

      iex> set = Effusion.IntSet.new(<<0 :: 1>>)
      iex> Enum.member?(set, 0)
      false

  You can also provide a list of integers.

      iex> set = Effusion.IntSet.new([0])
      iex> Enum.member?(set, 0)
      true

  """
  def new(bitfield)

  def new(bitfield) when is_bitstring(bitfield) do
    %IntSet{s: bitfield}
  end

  def new(list) when is_list(list) do
    Enum.into(list, IntSet.new())
  end

  @doc """
  Create a new int set containing the given element.

  ## Examples

      iex> set = Effusion.IntSet.singleton(0)
      iex> Enum.member?(set, 0)
      true

      iex> set = Effusion.IntSet.singleton(8)
      iex> Enum.member?(set, 8)
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
      iex> Enum.member?(u, 7)
      true
      iex> Enum.member?(u, 4)
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
  Add a value to the int set

  ## Examples

      iex> set = Effusion.IntSet.new()
      iex> set = Effusion.IntSet.put(set, 0)
      iex> Enum.member?(set, 0)
      true

  """
  def put(s, x)

  def put(%IntSet{s: s} = set, x) when is_index(x) and is_bitstring(s) do
    set_bit(set, x, 1)
  end

  def delete(%IntSet{s: s} = set, x) when is_index(x) and is_bitstring(s) and not can_contain(s, x) do
    set
  end

  def delete(%IntSet{s: s} = set, x) when can_contain(s, x) do
    set_bit(set, x, 0)
  end

  defp set_bit(%IntSet{s: s} = set, i, x) when x in 0..1 do
    %IntSet{s: s} = ensure_capacity_for(set, i)
    <<pre :: size(i), _ :: 1, post :: bitstring>> = s
    %IntSet{s: <<pre :: size(i), x :: 1, post :: bitstring>>}
  end

  defp ensure_capacity_for(%IntSet{s: s} = set, x) when can_contain(s, x) do
    set
  end

  defp ensure_capacity_for(%IntSet{s: s} = set, x) when is_index(x) and bit_size(s) <= x do
    total_bits_needed = x + 1
    bits_to_add = total_bits_needed - bit_size(s)
    %IntSet{s: <<s :: bitstring, 0 :: size(bits_to_add)>>}
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(s, opts) do
      int_list = Enum.into(s, []) |> Enum.sort()
      concat(["#IntSet<", to_doc(int_list, opts), ">"])
    end
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

    defguard is_index(i)
      when is_integer(i)
       and i >= 0

    def member?(%IntSet{}, x) when is_integer(x) and x < 0, do: {:ok, false}
    def member?(%IntSet{s: s}, x) when is_index(x) and bit_size(s) <= x, do: {:ok, false}
    def member?(%IntSet{s: <<0 :: 1, _rst :: bitstring>>}, 0), do: {:ok, false}
    def member?(%IntSet{s: <<1 :: 1, _rst :: bitstring>>}, 0), do: {:ok, true}

    def member?(%IntSet{s: s}, x)
    when is_index(x)
     and bit_size(s) > x
    do
      <<_ :: size(x), i :: 1, _ :: bitstring>> = s
      {:ok, i == 1}
    end

    def member?(%IntSet{}, x), do: {:error, __MODULE__}

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
