defmodule Effusion.THP do
  @moduledoc """
  Behavior defining the contract of the Tracker HTTP Protocol.
  """

  @type peer :: %{
    ip: :inet.hostname() | :inet.ip_address(),
    port: :inet.port_number(),
    peer_id: Effusion.peer_id()
  }

  @type compact_peer :: %{
    ip: :inet.ip4_address(),
    port: :inet.port_number()
  }

  @type tracker_response :: %{
    interval: pos_integer(),
    peers: [peer] | [compact_peer]
  }

  @callback announce(
    tracker_url :: String.t() | :unicode.unicode_binary(),
    peer_host :: :inet.hostname() | :inet.ip_address(),
    peer_port :: :inet.port_number(),
    peer_id :: Effusion.peer_id(),
    info_hash :: Effusion.info_hash(),
    uploaded :: non_neg_integer(),
    downloaded :: non_neg_integer(),
    left :: non_neg_integer(),
    event :: :started | :stopped | :completed | :interval
  ) :: {:ok, tracker_response()} | {:error, any()}
end
