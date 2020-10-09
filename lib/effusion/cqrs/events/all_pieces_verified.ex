defmodule Effusion.CQRS.Events.AllPiecesVerified do
  @enforce_keys [
    :info_hash
  ]
  defstruct [
    :info_hash
  ]
end
