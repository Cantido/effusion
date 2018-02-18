defmodule Effusion.BTP.Block do
  defguard is_index(i) when is_integer(i) and i >= 0
  defguard is_size(x) when is_integer (x) and x > 0

  def new(i, o, d) when is_index(i) and is_index(o) and is_binary(d) do
    %{index: i, offset: o, data: d}
  end

  def sequential?(%{index: i1, offset: o1, data: d1}, %{index: i2, offset: o2}) do
    (i1 == i2) and (o1 + byte_size(d1) == o2)
  end

  def adjacent?(b1, b2) do
    sequential?(b1, b2) or sequential?(b2, b1)
  end

  def merge(b1 = %{index: i1, offset: o1, data: d1}, b2 = %{index: i2, offset: o2, data: d2}) do
    cond do
      sequential?(b1, b2) ->
        %{
          index: i1,
          offset: o1,
          data: d1 <> d2
        }
      sequential?(b2, b1) ->
        %{
          index: i2,
          offset: o2,
          data: d2 <> d1
        }
    end
  end

  def finished?(block, target_size) do
    size(block) == target_size
  end

  def size(%{data: d}) do
    byte_size(d)
  end

  def size(%{size: s}) do
    s
  end

  def to_piece(%{index: i, offset: _, data: d}) do
    %{index: i, data: d}
  end
end
