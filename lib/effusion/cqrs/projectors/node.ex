defmodule Effusion.CQRS.Projectors.Node do
  use Commanded.Projections.Ecto,
    application: Effusion.CQRS.Application,
    repo: Effusion.Repo,
    name: "node"

  alias Effusion.CQRS.Projections.Node, as: NodeProjection
  alias Effusion.CQRS.Events.{
    DHTNodeAdded
  }
  require Logger

  project %DHTNodeAdded{node_id: node_id, host: host, port: port}, fn multi ->
    Ecto.Multi.insert(
      multi,
      :node,
      %NodeProjection{
        node_id: node_id,
        host: host,
        port: port
      })
  end

  def error({:error, error}, event, _failure_context) do
    Logger.error("Failed to project event #{inspect event}, reason: #{inspect error}")
    :skip
  end
end
