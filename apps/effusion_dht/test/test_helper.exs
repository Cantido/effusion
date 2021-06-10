ExUnit.start(capture_log: true)
ExUnit.configure(timeout: 10_000)

defmodule TestHelper do
  alias Effusion.DHT.Node

  def mint_info_hash do
    # This value was obtained from a Transmission torrent client, so this
    # value is the known-correct info_hash for the Mint 18.3 64-bit torrent.
    <<210, 229, 63, 182, 3, 101, 45, 153, 25, 145, 182, 173, 35, 87, 167, 162, 132, 90, 83, 25>>
  end

  def mint_meta do
    {:ok, metabin} = File.read("test/linuxmint-18.3-cinnamon-64bit.iso.torrent")
    {:ok, meta} = Metatorrent.decode(metabin)
    meta
  end

  def tiny_meta do
    %Metatorrent.Metainfo{
      announce: "http://localhost:6969/announce",
      created_by: "Lovingly hand-crafted, by Rosa <3",
      creation_date: ~U[2018-02-04 23:04:36Z],
      info: %Metatorrent.SingleFileInfo{
        :length => 5,
        :name => "tiny.txt",
        :piece_length => 3,
        :pieces => [
          <<242, 105, 25, 118, 134, 197, 108, 67, 163, 82, 84, 216, 119, 167, 25, 148, 192, 181,
            112, 48>>,
          <<144, 99, 169, 240, 224, 50, 182, 35, 148, 3, 183, 25, 203, 187, 165, 106, 196, 228,
            228, 95>>
        ]
      },
      info_hash:
        <<95, 189, 143, 1, 37, 56, 146, 40, 140, 78, 2, 250, 208, 144, 217, 10, 49, 7, 64, 28>>
    }
  end

  def generate_node do
    Node.generate_node_id()
    |> generate_node()
  end

  def generate_node(id) do
    %Node{
      id: id,
      host: {127, 0, 0, 1},
      port: Enum.random(1024..65535)
    }
    |> Node.response_received_at(~U[2021-04-18T14:11:00Z])
  end
end
