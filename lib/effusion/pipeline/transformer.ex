defmodule Effusion.Pipeline.Transformer do
  def transform(data, opts \\ []) do
    %Broadway.Message{data: data, acknowledger: nil}
  end
end
