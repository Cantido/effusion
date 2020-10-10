defimpl Jason.Encoder, for: [MapSet, Range, Stream, IntSet] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end
