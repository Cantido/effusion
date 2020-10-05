defmodule Effusion.Pipeline.Transformer do
  def transform(data, opts \\ []) do
    %Broadway.Message{
      data: data,
      acknowledger: {Broadway.NoopAcknowledger, nil, nil}
    }
  end
end
