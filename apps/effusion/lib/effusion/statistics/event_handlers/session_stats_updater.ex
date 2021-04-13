defmodule Effusion.Statistics.EventHandlers.SessionStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.Commanded,
    name: __MODULE__

  alias Effusion.Statistics.Session, as: SessionStats

  alias Effusion.PWP.Messages.Incoming.Events.{
    PeerChokedUs,
    PeerUnchokedUs,
    PeerInterestedInUs,
    PeerUninterestedInUs,
    PeerHasPiece,
    PeerHasBitfield,
    PeerRequestedBlock,
    PeerSentBlock,
    PeerCancelledRequest
  }

  alias Effusion.PWP.Messages.Outgoing.Events.{
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }

  require Logger

  def handle(%PeerChokedUs{}, _metadata) do
    SessionStats.inc_incoming_choke()
    :ok
  end

  def handle(%PeerUnchokedUs{}, _metadata) do
    SessionStats.inc_incoming_unchoke()
    :ok
  end

  def handle(%PeerInterestedInUs{}, _metadata) do
    SessionStats.inc_incoming_interested()
    :ok
  end

  def handle(%PeerUninterestedInUs{}, _metadata) do
    SessionStats.inc_incoming_uninterested()
    :ok
  end

  def handle(%PeerHasPiece{}, _metadata) do
    SessionStats.inc_incoming_have()
    :ok
  end

  def handle(%PeerHasBitfield{}, _metadata) do
    SessionStats.inc_incoming_bitfield()
    :ok
  end

  def handle(%PeerRequestedBlock{}, _metadata) do
    SessionStats.inc_incoming_request()
    :ok
  end

  def handle(%PeerSentBlock{}, _metadata) do
    SessionStats.inc_incoming_piece()
    :ok
  end

  def handle(%PeerCancelledRequest{}, _metadata) do
    SessionStats.inc_incoming_cancel()
    :ok
  end

  def handle(%BitfieldSent{}, _metadata) do
    SessionStats.inc_outgoing_bitfield()
    :ok
  end

  def handle(%BlockRequested{}, _metadata) do
    SessionStats.inc_outgoing_request()
    :ok
  end

  def handle(%InterestedSent{}, _metadata) do
    SessionStats.inc_outgoing_interested()
    :ok
  end

  def handle(%RequestCancelled{}, _metadata) do
    SessionStats.inc_outgoing_cancel()
    :ok
  end

  def handle(%SendingHave{}, _metadata) do
    SessionStats.inc_outgoing_cancel()
    :ok
  end

  def error(error, failed_event, _context) do
    Logger.error(
      "SessionStatsUpdater failed to process event #{inspect(failed_event)} due to error #{
        inspect(error)
      }"
    )

    :skip
  end
end
