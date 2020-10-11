defimpl Jason.Encoder, for: [MapSet, Range, Stream] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end

defimpl Jason.Encoder, for: [IntSet] do
  def encode(struct, opts) do
    struct
    |> IntSet.bitstring(byte_align: true)
    |> Base.encode16(case: :lower)
    |> Jason.Encode.string(opts)
  end
end
