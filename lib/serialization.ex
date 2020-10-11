defimpl Jason.Encoder, for: [MapSet, Range, Stream] do
  def encode(struct, opts) do
    Jason.Encode.list(Enum.to_list(struct), opts)
  end
end

defimpl Jason.Encoder, for: [IntSet] do
  def encode(struct, opts) do
    Jason.Encode.string(Base.encode16(struct, case: :lower), opts)
  end
end
