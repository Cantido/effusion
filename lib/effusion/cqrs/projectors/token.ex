defmodule Effusion.CQRS.Projectors.Token do
  use Commanded.Projections.Ecto,
    application: Effusion.CQRS.Application,
    repo: Effusion.Repo,
    name: "token"

  alias Effusion.CQRS.Projections.Token, as: TokenProjection
  alias Effusion.CQRS.Events.{
    TokenIssued
  }
  require Logger

  project %TokenIssued{
    node_id: node_id,
    info_hash: info_hash,
    token: token,
    expires_at: expires_at
  }, fn multi ->
    Ecto.Multi.insert(
      multi,
      :token,
      %TokenProjection{
        issued_to: node_id,
        info_hash: info_hash,
        value: token,
        expires_at: expires_at
      })
  end

  def error({:error, error}, event, _failure_context) do
    Logger.error("Failed to project event #{inspect event}, reason: #{inspect error}")
    :skip
  end
end
