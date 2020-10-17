defmodule Effusion.DHT.Tokens do
  alias Effusion.CQRS.Application, as: CQRS
  alias Effusion.CQRS.Commands.{
    IssueToken,
    AcceptToken
  }

  def issue_token(node_id, info_hash, token, expiry) do
    CQRS.dispatch(
      %IssueToken{
        node_id: Effusion.Hash.encode(node_id),
        info_hash: Effusion.Hash.encode(info_hash),
        token: token,
        expires_at: expiry
      }
    )
  end

  def accept_token(node_id, info_hash, token, expires_at) do
    CQRS.dispatch(
      %AcceptToken{
        node_id: Effusion.Hash.encode(node_id),
        info_hash: Effusion.Hash.encode(info_hash),
        token: token,
        expires_at: expires_at
      }
    )
  end
end
