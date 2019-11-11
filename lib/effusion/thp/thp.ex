defmodule Effusion.THP do
  @moduledoc """
  Behavior defining the contract of the Tracker HTTP Protocol.
  """

  @typedoc """
  A peer returned by a tracker.
  """
  @type peer :: %{
          ip: :inet.hostname() | :inet.ip_address(),
          port: :inet.port_number(),
          peer_id: Effusion.peer_id()
        }

  @typedoc """
  A peer returned by a tracker when it was called with the `compact` option set.
  """
  @type compact_peer :: %{
          ip: :inet.ip4_address(),
          port: :inet.port_number()
        }

  @typedoc """
  A successful response from from a tracker.
  """
  @type tracker_response :: %{
          interval: pos_integer(),
          peers: [peer] | [compact_peer],
          trackerid: String.t()
        }

  @typedoc """
  Optional tracker parameters
  """
  @type tracker_opts :: [
    compact: (0 | 1),
    event: (:started | :stopped | :completed | :interval),
    ip: :inet.ip_address(),
    key: String.t(),
    no_peer_id: (0 | 1),
    numwant: non_neg_integer(),
    trackerid: String.t()
  ]

  @doc """
  Announce an event to the given tracker.
  """
  @callback announce(
              tracker_url :: String.t() | :unicode.unicode_binary(),
              peer_host :: :inet.hostname() | :inet.ip_address(),
              peer_port :: :inet.port_number(),
              peer_id :: Effusion.peer_id(),
              info_hash :: Effusion.info_hash(),
              uploaded :: non_neg_integer(),
              downloaded :: non_neg_integer(),
              left :: non_neg_integer(),
              opts :: tracker_opts()
            ) :: {:ok, tracker_response()} | {:error, any()}
end
