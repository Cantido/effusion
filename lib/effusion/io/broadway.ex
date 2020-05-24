defmodule Effusion.IO.Broadway do
  alias Broadway.Message
  use Broadway

  @moduledoc """
  Functions for reading and writing files described by torrents.
  """

  def start_link(_opts) do
    Broadway.start_link(__MODULE__,
      name: __MODULE__,
      producer: [
        module: {Effusion.IO.Server, []}
      ],
      processors: [
        default: []
      ]
    )
  end

  @doc """
  Consume a GenStage event to write a piece.
  """
  @impl true
  def handle_message(_, message, _context) do
    try do
      Effusion.IO.write_piece(message.data)
      message
    rescue
      reason -> Message.failed(message, reason)
    end
  end
end
