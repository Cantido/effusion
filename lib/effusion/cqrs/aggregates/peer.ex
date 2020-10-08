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
    internal_peer_id: nil,
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
    %__MODULE__{internal_peer_id: nil},
    %AddPeer{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, from: source}
  ) do
    %PeerAdded{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, from: source}
  end

  def execute(%__MODULE__{}, %AddPeer{}) do
    {:error, :peer_already_exists}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id, host: host, port: port},
    %AttemptToConnect{internal_peer_id: internal_peer_id}
  ) do
    %AttemptingToConnect{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port}
  end

  def execute(
    %__MODULE__{},
    %AddConnectedPeer{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, initiated_by: initiated_by}
  ) do
    %PeerConnected{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, initiated_by: initiated_by}
  end

  def execute(
    %__MODULE__{},
    %RemoveConnectedPeer{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, reason: reason}
  ) do
    %PeerDisconnected{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port, reason: reason}
  end

  def execute(
    %__MODULE__{},
    %SendHandshake{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :them
    }
  ) do
    [
      %SendingHandshake{
        internal_peer_id: internal_peer_id,
        info_hash: info_hash,
        peer_id: peer_id,
        host: host,
        port: port,
        our_peer_id: our_peer_id,
        our_extensions: our_extensions,
        initiated_by: :them
      },
      %SuccessfulHandshake{
        internal_peer_id: internal_peer_id,
        info_hash: info_hash,
        peer_id: peer_id,
        host: host,
        port: port,
        initiated_by: :them
      }
    ]
  end

  def execute(
    %__MODULE__{},
    %SendHandshake{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :us
    }
  ) do
    %SendingHandshake{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      our_peer_id: our_peer_id,
      our_extensions: our_extensions,
      initiated_by: :us}
  end

  def execute(
    %__MODULE__{},
    %HandleHandshake{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port,
      initiated_by: initiated_by
    }
  ) do
    if initiated_by == :us do
      %SuccessfulHandshake{
        internal_peer_id: internal_peer_id,
        info_hash: info_hash,
        peer_id: peer_id,
        host: host,
        port: port,
        initiated_by: initiated_by
      }
    else
      %PeerSentHandshake{
        internal_peer_id: internal_peer_id,
        info_hash: info_hash,
        peer_id: peer_id,
        host: host,
        port: port,
        initiated_by: initiated_by
      }
    end
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleChoke{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  ) do
    %PeerChokedUs{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleUnchoke{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  ) do
    %PeerUnchokedUs{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleInterested{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  ) do
    %PeerInterestedInUs{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleUninterested{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  ) do
    %PeerUninterestedInUs{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id, bitfield: bitfield},
    %HandleHave{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index}
  ) do
    %PeerHasPiece{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, bitfield: IntSet.put(bitfield, index)}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandlePiece{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, data: data}
  ) do
    %PeerSentBlock{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, data: data}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleBitfield{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  ) do
    %PeerHasBitfield{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, bitfield: bitfield}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleRequest{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  ) do
    %PeerRequestedBlock{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %HandleCancel{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  ) do
    %PeerRequestCancelled{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %SendInterested{internal_peer_id: internal_peer_id}
  ) do
    %InterestedSent{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id}
  end

  def execute(
    %__MODULE__{info_hash: info_hash, peer_id: peer_id},
    %RequestBlock{internal_peer_id: internal_peer_id, index: index, offset: offset, size: size}
  ) do
    %BlockRequested{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, index: index, offset: offset, size: size}
  end

  def execute(
    %__MODULE__{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id
    },
    %SendBitfield{bitfield: bitfield}
  ) do
    %BitfieldSent{
      internal_peer_id: internal_peer_id,
      info_hash: info_hash,
      peer_id: peer_id,
      bitfield: bitfield
    }
  end

  def apply(
    %__MODULE__{info_hash: nil, host: nil, port: nil} = peer,
    %PeerAdded{internal_peer_id: internal_peer_id, info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  ) do
    %__MODULE__{peer |
      internal_peer_id: internal_peer_id,
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
    %PeerConnected{info_hash: info_hash, peer_id: peer_id, host: host, port: port}
  ) do
    %__MODULE__{peer |
      info_hash: info_hash,
      peer_id: peer_id,
      host: host,
      port: port
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
