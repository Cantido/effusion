defmodule Effusion.PWP.GenPeer do
  @callback handle_handshake({<<_::160>>, <<_::160>>, <<_::64>>}, term)
    :: {:ok, {<<_::160>>, <<_::160>>, <<_::64>>}, term} |
       {:error, :bad_handshake} |
       {:error, :remote_same_as_local} |
       {:error, :unknown_info_hash}
end
