defmodule Effusion.InfoHash do
  @moduledoc """
  Manipulate info hashes.
  """

  @typedoc """
  An info hash binary. This is the 20-byte SHA1 hash of the completed
  download file, used to identify the torrent and to check the final download
  for integrity.
  """
  @type t :: <<_::160>>
end
