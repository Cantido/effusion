defmodule Effusion.PWP.GenPeer do
  @type state :: term
  @type handshake :: {<<_::160>>, <<_::160>>, <<_::64>>}
  @type intset :: integer

  @callback init() :: state

  @callback handle_handshake(handshake, state)
    :: {:ok, handshake, state} |
       {:ok, handshake, intset, state} |
       {:error, :bad_handshake} |
       {:error, :remote_same_as_local} |
       {:error, :unknown_info_hash}

  @callback handle_msg(term, state) :: {:ok, state} | :error

  @callback choke(state) :: {:ok, state} | :error
  @callback unchoke(state) :: {:ok, state} | :error
  @callback interested(state) :: {:ok, state} | :error
  @callback uninterested(state) :: {:ok, state} | :error
  @callback have(term, state) :: {:ok, state} | {:ok, :interested, state} | :error
  @callback bitfield(term, state) :: {:ok, state} | :error
  @callback request(term, state) :: {:ok, state} | :error
  @callback piece(term, state) :: {:ok, state} | :error
  @callback cancel(term, state) :: {:ok, state} | :error
end
