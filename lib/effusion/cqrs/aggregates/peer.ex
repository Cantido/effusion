defmodule Effusion.CQRS.Aggregates.Peer do
  alias Effusion.CQRS.Commands.{
    AddPeer,
    AttemptToConnect,
    AddOpenedPeerConnection,
    AddConnectedPeer,
    RemoveConnectedPeer,
    HandleBitfield,
    HandleCancel,
    HandleChoke,
    HandleHave,
    HandleHandshake,
    HandleInterested,
    HandlePiece,
    HandleRequest,
    HandleUnchoke,
    HandleInterested,
    HandleUninterested,
    SendInterested,
    RequestBlock,
    SendBitfield,
    SendHandshake,
    SendHave
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    PeerAdded,
    PeerConnectionOpened,
    PeerConnected,
    PeerDisconnected,
    PeerChokedUs,
    PeerUnchokedUs,
    PeerInterestedInUs,
    PeerUninterestedInUs,
    PeerHasPiece,
    PeerHasBitfield,
    PeerRequestedBlock,
    PeerRequestCancelled,
    PeerSentBlock,
    SuccessfulHandshake,
    InterestedSent,
    BlockRequested,
    BitfieldSent,
    SendingHandshake,
    FailedHandshake,
    PeerSentHandshake,
    SendingHave
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
    am_choking: true,
    am_interested: false,
    peer_choking: true,
    peer_interested: false
  ]

  def execute(
    %__MODULE__{peer_uuid: nil},
    %AddPeer{peer_uuid: peer_uuid, expected_info_hash: info_hash, expected_peer_id: peer_id, host: host, port: port, from: source}
  ) do
    %PeerAdded{peer_uuid: peer_uuid, expected_info_hash: info_hash, expected_peer_id: peer_id, host: host, port: port, from: source}
  end

  def execute(%__MODULE__{}, %AddPeer{}) do
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
    %__MODULE__{expected_info_hash: info_hash, host: host, port: port},
    %AddOpenedPeerConnection{peer_uuid: peer_uuid}
  ) do
    %PeerConnectionOpened{
      peer_uuid: peer_uuid
    }
  end

  def execute(
    %__MODULE__{info_hash: info_hash},
    %AddConnectedPeer{peer_uuid: peer_uuid, initiated_by: initiated_by}
  ) do
    Logger.debug("***** Emitting PeerConnected event")
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
    if initiated_by == :them do
      [
        %SendingHandshake{
          peer_uuid: peer_uuid,
          info_hash: info_hash,
          our_peer_id: our_peer_id,
          our_extensions: our_extensions,
          initiated_by: :them
        },
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: :them
        }
      ]
    else
      %SendingHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: :us}
      end
  end

  def execute(aggregate, %HandleHandshake{} = command) do
    aggregate
    |> Multi.new()
    |> Multi.execute(&handle_handshake(&1, command))
    |> Multi.execute(&check_handshake_params(&1, command))
  end

  defp handle_handshake(
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
      initiated_by: :us,
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
      true ->
        %SuccessfulHandshake{
          peer_uuid: peer_uuid,
          initiated_by: :us
        }
    end
  end

  def check_handshake_params(
    %__MODULE__{
      peer_uuid: peer_uuid,
      expected_info_hash: expected_info_hash,
      expected_peer_id: expected_peer_id
    },
    %HandleHandshake{
      initiated_by: :them,
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
      true -> nil
    end
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
    %PeerRequestCancelled{peer_uuid: peer_uuid, info_hash: info_hash, index: index, offset: offset, size: size}
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
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash},
    %RequestBlock{index: index, offset: offset, size: size}
  ) do
    %BlockRequested{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      index: index,
      offset:
      offset,
      size: size
    }
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
    %PeerConnectionOpened{}
  ) do
    peer
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
    peer
  end

  def apply(%__MODULE__{} = peer, %PeerDisconnected{}) do
    peer
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
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerSentBlock{}
  ) do
    peer
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
    %__MODULE__{} = peer,
    %BitfieldSent{}
  ) do
    peer
  end
end
