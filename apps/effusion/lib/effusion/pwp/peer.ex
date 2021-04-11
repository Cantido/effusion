defmodule Effusion.PWP.Peer do
  alias Effusion.PWP.Commands.Connection.{
    AddConnectedPeer,
    AddOpenedPeerConnection,
    AttemptToConnect,
    HandleFailedConnectionAttempt,
    RemoveConnectedPeer
  }
  alias Effusion.PWP.Commands.Handshake.{
    HandleHandshake,
    SendHandshake,
    TimeoutHandshake
  }
  alias Effusion.PWP.Commands.Incoming.{
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHave,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleUninterested
  }
  alias Effusion.PWP.Commands.Outgoing.{
    CancelRequest,
    RequestBlock,
    SendBitfield,
    SendHave,
    SendInterested
  }
  alias Effusion.PWP.Commands.Swarm.AddPeerAddress
  alias Effusion.PWP.Events.Incoming.{
    PeerCancelledRequest,
    PeerChokedUs,
    PeerHasBitfield,
    PeerHasPiece,
    PeerInterestedInUs,
    PeerRequestedBlock,
    PeerSentBlock,
    PeerSentHandshake,
    PeerUnchokedUs,
    PeerUninterestedInUs,
    SuccessfulHandshake
  }
  alias Effusion.PWP.Events.Outgoing.{
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    ConnectionAttemptFailed,
    PeerAdded,
    PeerConnectionOpened,
    PeerConnected,
    PeerDisconnected,
    SendingHandshake,
    FailedHandshake,
  }
  alias Commanded.Aggregate.Multi
  require Logger

  defstruct [
    peer_uuid: nil,
    expected_info_hash: nil,
    expected_peer_id: nil,
    info_hash: nil,
    peer_id: nil,
    host: nil,
    port: nil,
    bitfield: IntSet.new(),
    our_requests: MapSet.new(),
    am_choking: true,
    am_interested: false,
    peer_choking: true,
    peer_interested: false,
    connection_state: "disconnected"
  ]

  def execute(
    %__MODULE__{peer_uuid: nil},
    %AddPeerAddress{peer_uuid: peer_uuid, expected_info_hash: info_hash, expected_peer_id: peer_id, host: host, port: port, from: source}
  ) do
    %PeerAdded{peer_uuid: peer_uuid, expected_info_hash: info_hash, expected_peer_id: peer_id, host: host, port: port, from: source}
  end

  def execute(%__MODULE__{}, %AddPeerAddress{}) do
    {:error, :peer_already_exists}
  end

  def execute(
    %__MODULE__{expected_info_hash: info_hash, host: host, port: port},
    %AttemptToConnect{peer_uuid: peer_uuid}
  ) do
    %AttemptingToConnect{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      host: host,
      port: port}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleFailedConnectionAttempt{reason: reason}
  ) do
    %ConnectionAttemptFailed{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      reason: reason
    }
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, connection_state: connection_state},
    %TimeoutHandshake{}
  ) do
    unless connection_state == "connected" do
      [
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: "handshake timed out"
        },
        %ConnectionAttemptFailed{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          reason: "handshake timed out"
        }
      ]
    end
  end

  def execute(
    %__MODULE__{},
    %AddOpenedPeerConnection{peer_uuid: peer_uuid, host: host, port: port}
  ) do
    %PeerConnectionOpened{
      peer_uuid: peer_uuid,
      host: host,
      port: port
    }
  end

  def execute(
    %__MODULE__{info_hash: info_hash},
    %AddConnectedPeer{peer_uuid: peer_uuid, initiated_by: initiated_by}
  ) do
    %PeerConnected{peer_uuid: peer_uuid, info_hash: info_hash, initiated_by: initiated_by}
  end

  def execute(
    %__MODULE__{expected_info_hash: info_hash},
    %RemoveConnectedPeer{peer_uuid: peer_uuid, reason: reason}
  ) do
    %PeerDisconnected{peer_uuid: peer_uuid, info_hash: info_hash, reason: reason}
  end

  def execute(
    %__MODULE__{expected_info_hash: info_hash},
    %SendHandshake{
      peer_uuid: peer_uuid,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: initiated_by
    }
  ) do
    if initiated_by == "them" do
      [
        %SendingHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          our_peer_id: our_peer_id,
          our_extensions: our_extensions,
          initiated_by: "them"
        },
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: "them"
        }
      ]
    else
      %SendingHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: "us"}
      end
  end

  def execute(aggregate, %HandleHandshake{} = command) do
    aggregate
    |> Multi.new()
    |> Multi.execute(&handle_handshake(&1, command))
    |> Multi.execute(&check_handshake_params(&1, command))
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleChoke{}
  ) do
    %PeerChokedUs{peer_uuid: peer_uuid, info_hash: info_hash}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleUnchoke{}
  ) do
    %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: info_hash}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleInterested{}
  ) do
    %PeerInterestedInUs{peer_uuid: peer_uuid, info_hash: info_hash}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleUninterested{}
  ) do
    %PeerUninterestedInUs{peer_uuid: peer_uuid, info_hash: info_hash}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, bitfield: bitfield},
    %HandleHave{index: index}
  ) do
    %PeerHasPiece{peer_uuid: peer_uuid, info_hash: info_hash, index: index, bitfield: IntSet.put(bitfield, index)}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandlePiece{index: index, offset: offset, data: data}
  ) do
    %PeerSentBlock{peer_uuid: peer_uuid, info_hash: info_hash, index: index, offset: offset, data: data}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleBitfield{bitfield: bitfield}
  ) do
    %PeerHasBitfield{peer_uuid: peer_uuid, info_hash: info_hash, bitfield: bitfield}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleRequest{index: index, offset: offset, size: size}
  ) do
    %PeerRequestedBlock{peer_uuid: peer_uuid, info_hash: info_hash, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %HandleCancel{index: index, offset: offset, size: size}
  ) do
    %PeerCancelledRequest{peer_uuid: peer_uuid, info_hash: info_hash, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %SendInterested{}
  ) do
    %InterestedSent{
      peer_uuid: peer_uuid,
      info_hash: info_hash
    }
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, bitfield: bitfield, our_requests: our_requests, peer_choking: peer_choking},
    %RequestBlock{index: index, offset: offset, size: size}
  ) do
    cond do
      peer_choking -> {:error, "Peer is choking us, we cannot send requests"}
      not Enum.member?(bitfield, index) -> {:error, "Cannot request a piece the peer does not have"}
      Enum.member?(our_requests, {index, offset, size}) -> {:error, "Already requested this piece from this peer"}
      true ->
        %BlockRequested{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          index: index,
          offset: offset,
          size: size
        }
    end
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, our_requests: our_requests},
    %CancelRequest{index: index, offset: offset, size: size}
  ) do
    if Enum.member?(our_requests, {index, offset, size}) do
      %RequestCancelled{peer_uuid: peer_uuid, info_hash: info_hash, index: index, offset: offset, size: size}
    else
      {:error, "piece was not requested, cannot cancel request"}
    end
  end

  def execute(
    %__MODULE__{
      peer_uuid: peer_uuid,
      info_hash: info_hash
    },
    %SendBitfield{bitfield: bitfield}
  ) do
    %BitfieldSent{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      bitfield: bitfield
    }
  end

  def execute(
    %__MODULE__{
      peer_uuid: peer_uuid
    },
    %SendHave{index: index}
  ) do
    %SendingHave{
      peer_uuid: peer_uuid,
      index: index
    }
  end

  defp handle_handshake(
    %__MODULE__{
      peer_uuid: peer_uuid
    },
    %HandleHandshake{
      initiated_by: initiated_by,
      info_hash: info_hash,
      peer_id: peer_id
    }
  ) do
    %PeerSentHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      initiated_by: initiated_by
    }
  end

  def check_handshake_params(
    %__MODULE__{
      peer_uuid: peer_uuid,
      expected_info_hash: expected_info_hash,
      expected_peer_id: expected_peer_id
    },
    %HandleHandshake{
      initiated_by: initiated_by,
      info_hash: info_hash,
      peer_id: peer_id
    }
  ) do
    cond do
      info_hash != expected_info_hash ->
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: :info_hash
        }
      peer_id != expected_peer_id ->
        %FailedHandshake{
          peer_uuid: peer_uuid,
          failure_reason: :peer_id
        }
      initiated_by == "us" ->
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: "us"
        }
      true -> nil
    end
  end

  def apply(
    %__MODULE__{info_hash: nil, host: nil, port: nil} = peer,
    %PeerAdded{peer_uuid: peer_uuid, expected_info_hash: info_hash, expected_peer_id: peer_id, host: host, port: port}
  ) do
    %__MODULE__{peer |
      peer_uuid: peer_uuid,
      expected_peer_id: peer_id,
      expected_info_hash: info_hash,
      info_hash: info_hash,
      host: host,
      port: port
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerConnectionOpened{host: host, port: port}
  ) do
    %__MODULE__{peer |
      host: host,
      port: port
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %ConnectionAttemptFailed{}
  ) do
    %__MODULE__{peer | connection_state: "disconnected"}
  end

  def apply(
    %__MODULE__{} = peer,
    %SendingHandshake{}
  ) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerSentHandshake{peer_id: peer_id, info_hash: info_hash}
  ) do
    %__MODULE__{peer |
      peer_id: peer_id,
      info_hash: info_hash
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %SuccessfulHandshake{}
  ) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %FailedHandshake{}
  ) do
    peer
  end

  def apply(%__MODULE__{} = peer, %AttemptingToConnect{}) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerConnected{}
  ) do
    %__MODULE__{peer | connection_state: "connected"}
  end

  def apply(%__MODULE__{} = peer, %PeerDisconnected{}) do
    %__MODULE__{peer | connection_state: "disconnected"}
  end

  def apply(
    %__MODULE__{} = peer,
    %SendingHave{}
  ) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerChokedUs{}
  ) do
    %__MODULE__{peer |
      peer_choking: true
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerUnchokedUs{}
  ) do
    %__MODULE__{peer |
      peer_choking: false
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerInterestedInUs{}
  ) do
    %__MODULE__{peer |
      peer_interested: true
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerUninterestedInUs{}
  ) do
    %__MODULE__{peer |
      peer_interested: false
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerHasPiece{bitfield: bitfield}
  ) do
    %__MODULE__{peer |
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerHasBitfield{bitfield: bitfield}
  ) do
    %__MODULE__{peer |
      bitfield: IntSet.new(Base.decode64!(bitfield))
    }
  end

  def apply(
    %__MODULE__{our_requests: our_requests} = peer,
    %PeerSentBlock{index: index, offset: offset}
  ) do
    %__MODULE__{peer |
      our_requests: MapSet.delete(our_requests, {index, offset, Application.fetch_env!(:effusion, :block_size)})
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %InterestedSent{}
  ) do
    %__MODULE__{peer |
      am_interested: true
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %BlockRequested{}
  ) do
    peer
  end

  def apply(
    %__MODULE__{our_requests: our_requests} = peer,
    %RequestCancelled{index: index, offset: offset, size: size}
  ) do
    %__MODULE__{peer |
      our_requests: MapSet.delete(our_requests, {index, offset, size})
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %BitfieldSent{}
  ) do
    peer
  end

  defimpl Commanded.Serialization.JsonDecoder, for: Effusion.PWP.Peer do
    def decode(
      %Effusion.PWP.Peer{
        bitfield: bitfield,
        our_requests: our_requests
      } = state
    ) do
      %Effusion.PWP.Peer{state |
        bitfield: IntSet.new(bitfield),
        our_requests: MapSet.new(our_requests)
      }
    end
  end
end
