defmodule Effusion.ComputeWorker do
  alias Effusion.ActiveTorrent

  def verify_piece(piece_data, piece_index, meta) do
    expected_hash = Enum.at(meta.info.pieces, piece_index)
    actual_hash = Effusion.Hash.calc(piece_data)

    if expected_hash == actual_hash do
      ActiveTorrent.piece_verified(meta.info_hash, piece_index)
    else
      ActiveTorrent.piece_failed_verification(meta.info_hash, piece_index)
    end
  end
end
