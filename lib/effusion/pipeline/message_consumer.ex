defmodule Effusion.Pipeline.MessageConsumer do
  use GenStage
  require Logger

  def start_link(_opts) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    {:consumer, 0, [subscribe_to: [MessageProducer]]}
  end

  @impl true
  def handle_events(events, _from, state) do
    Enum.each(events, &Effusion.PWP.ProtocolHandler.handle_message/1)
    {:noreply, [], state}
  end
end
