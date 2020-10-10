defmodule Effusion.CQRS.EventHandlers.SessionStatsUpdater do
  use Commanded.Event.Handler,
    application: Effusion.CQRS.Application,
    name: __MODULE__

  alias Effusion.Statistics.Session, as: SessionStats
  alias Effusion.CQRS.Events.{
    PeerChokedUs,
    PeerUnchokedUs,
    PeerInterestedInUs,
    PeerUninterestedInUs,
    PeerHasPiece,
    PeerHasBitfield,
    PeerRequestedBlock,
    PeerSentBlock,
    PeerCancelledRequest,
    BitfieldSent,
    BlockRequested,
    InterestedSent,
    RequestCancelled,
    SendingHave
  }
  require Logger

  def handle(%PeerChokedUs{},_metadata), do: SessionStats.inc_incoming_choke()
  def handle(%PeerUnchokedUs{},_metadata), do: SessionStats.inc_incoming_unchoke()
  def handle(%PeerInterestedInUs{},_metadata), do: SessionStats.inc_incoming_interested()
  def handle(%PeerUninterestedInUs{},_metadata), do: SessionStats.inc_incoming_uninterested()
  def handle(%PeerHasPiece{},_metadata), do: SessionStats.inc_incoming_have()
  def handle(%PeerHasBitfield{},_metadata), do: SessionStats.inc_incoming_bitfield()
  def handle(%PeerRequestedBlock{},_metadata), do: SessionStats.inc_incoming_request()
  def handle(%PeerSentBlock{},_metadata), do: SessionStats.inc_incoming_piece()
  def handle(%PeerCancelledRequest{},_metadata), do: SessionStats.inc_incoming_cancel()


  def handle(%BitfieldSent{},_metadata), do: SessionStats.inc_outgoing_bitfield()
  def handle(%BlockRequested{},_metadata), do: SessionStats.inc_outgoing_piece()
  def handle(%InterestedSent{},_metadata), do: SessionStats.inc_outgoing_interested()
  def handle(%RequestCancelled{},_metadata), do: SessionStats.inc_outgoing_cancel()
  def handle(%SendingHave{},_metadata), do: SessionStats.inc_outgoing_cancel()

  def error(error, failed_event, _context) do
    Logger.error("SessionStatsUpdater failed to process event #{inspect failed_event} due to error #{inspect error}")
    :skip
  end
end
