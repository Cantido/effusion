defmodule Effusion.THP do
  @type peer :: %{
    ip: any(),
    port: any()
  }

  @type tracker_response :: %{
    interval: pos_integer(),
    peers: [peer]
  }

  @callback announce(
    tracker_url :: any(),
    peer_host :: any(),
    peer_port :: any(),
    peer_id :: any(),
    info_hash :: any(),
    uploaded :: any(),
    downloaded :: any(),
    left :: any()
  ) :: {:ok, tracker_response()} | {:error, any()}
end
