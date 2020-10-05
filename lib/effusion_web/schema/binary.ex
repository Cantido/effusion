defmodule Effusion.Schema.Binary do
  # use Absinthe.Schema.Notation
  #
  # @moduledoc """
  # Absinthe schema for serializing binaries.
  # """
  #
  # scalar :binary, name: "Binary" do
  #   serialize &(Base.encode64(&1))
  #   parse &decode/1
  # end
  #
  # def decode(%Absinthe.Blueprint.Input.String{value: value}) do
  #   Base.decode64(value)
  # end
end
