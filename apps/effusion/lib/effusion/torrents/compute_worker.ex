defmodule Effusion.ComputeWorker do
  alias Effusion.Torrents

  def verify_piece(piece_data, piece_index, meta) do
    expected_hash = Enum.at(meta.info.pieces, piece_index)
    actual_hash = Effusion.Hash.calc(piece_data)

    if expected_hash == actual_hash do
      Torrents.piece_verified(meta.info_hash, piece_index)
    else
      Torrents.piece_failed_verification(meta.info_hash, piece_index)
    end
  end
end
