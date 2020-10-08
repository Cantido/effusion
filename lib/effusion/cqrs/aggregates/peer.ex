defmodule Effusion.CQRS.Aggregates.Peer do
  alias Effusion.CQRS.Commands.{
    AddPeer,
    AttemptToConnect,
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
    SendHandshake
  }
  alias Effusion.CQRS.Events.{
    AttemptingToConnect,
    PeerAdded,
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
    PeerSentHandshake
  }

  defstruct [
    peer_uuid: nil,
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
    %AddPeer{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, host: host, port: port, from: source}
  ) do
    %PeerAdded{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, host: host, port: port, from: source}
  end

  def execute(%__MODULE__{}, %AddPeer{}) do
    {:error, :peer_already_exists}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id, host: host, port: port},
    %AttemptToConnect{peer_uuid: peer_uuid}
  ) do
    %AttemptingToConnect{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port}
  end

  def execute(
    %__MODULE__{},
    %AddConnectedPeer{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, initiated_by: initiated_by}
  ) do
    %PeerConnected{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, initiated_by: initiated_by}
  end

  def execute(
    %__MODULE__{},
    %RemoveConnectedPeer{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, reason: reason}
  ) do
    %PeerDisconnected{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, reason: reason}
  end

  def execute(
    %__MODULE__{},
    %SendHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :them
    }
  ) do
    [
      %SendingHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: peer_id,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: :them
      },
      %SuccessfulHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: peer_id,
        initiated_by: :them
      }
    ]
  end

  def execute(
    %__MODULE__{},
    %SendHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :us
    }
  ) do
    %SendingHandshake{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :us}
  end

  def execute(
    %__MODULE__{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id
    },
    %HandleHandshake{
      initiated_by: initiated_by
    }
  ) do
    if initiated_by == :us do
      %SuccessfulHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: peer_id,
        initiated_by: initiated_by
      }
    else
      %PeerSentHandshake{
        peer_uuid: peer_uuid,
        info_hash: info_hash,
        peer_id: peer_id,
        initiated_by: initiated_by
      }
    end
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleChoke{}
  ) do
    %PeerChokedUs{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleUnchoke{}
  ) do
    %PeerUnchokedUs{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleInterested{}
  ) do
    %PeerInterestedInUs{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleUninterested{}
  ) do
    %PeerUninterestedInUs{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, bitfield: bitfield},
    %HandleHave{index: index}
  ) do
    %PeerHasPiece{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, index: index, bitfield: IntSet.put(bitfield, index)}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandlePiece{index: index, offset: offset, data: data}
  ) do
    %PeerSentBlock{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, data: data}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleBitfield{bitfield: bitfield}
  ) do
    %PeerHasBitfield{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleRequest{index: index, offset: offset, size: size}
  ) do
    %PeerRequestedBlock{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %HandleCancel{index: index, offset: offset, size: size}
  ) do
    %PeerRequestCancelled{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %SendInterested{}
  ) do
    %InterestedSent{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id},
    %RequestBlock{index: index, offset: offset, size: size}
  ) do
    %BlockRequested{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id
    },
    %SendBitfield{bitfield: bitfield}
  ) do
    %BitfieldSent{
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{info_hash: nil, host: nil, port: nil} = peer,
    %PeerAdded{peer_uuid: peer_uuid, info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  ) do
    %__MODULE__{peer |
      peer_uuid: peer_uuid,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port
    }
  end

  def apply(%__MODULE__{} = peer, %AttemptingToConnect{}) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %PeerConnected{info_hash: info_hash, peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      info_hash: info_hash,
      peer_id: peer_id
    }
  end

  def apply(%__MODULE__{} = peer, %PeerDisconnected{}) do
    peer
  end

  def apply(
    %__MODULE__{info_hash: info_hash} = peer,
    %PeerSentHandshake{peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_id: peer_id
    }
  end

  def apply(
    %__MODULE__{} = peer,
    %SendingHandshake{}
  ) do
    peer
  end

  def apply(
    %__MODULE__{} = peer,
    %SuccessfulHandshake{peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_id: peer_id
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerChokedUs{info_hash: info_hash, peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_choking: true
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerUnchokedUs{info_hash: info_hash, peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_choking: false
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerInterestedInUs{info_hash: info_hash, peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_interested: true
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerUninterestedInUs{info_hash: info_hash, peer_id: peer_id}
  ) do
    %__MODULE__{peer |
      peer_interested: false
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerHasPiece{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  ) do
    %__MODULE__{peer |
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerHasBitfield{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  ) do
    %__MODULE__{peer |
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id} = peer,
    %PeerSentBlock{info_hash: info_hash, peer_id: peer_id}
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
