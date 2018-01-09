defmodule Effusion.PWP.GenPeer do
  @type state :: term

  @callback init() :: state

  @callback handle_handshake({<<_::160>>, <<_::160>>, <<_::64>>}, state)
    :: {:ok, {<<_::160>>, <<_::160>>, <<_::64>>}, state} |
       {:error, :bad_handshake} |
       {:error, :remote_same_as_local} |
       {:error, :unknown_info_hash}

  @callback handle_msg(term, state) :: {:ok, state} | :error
end
