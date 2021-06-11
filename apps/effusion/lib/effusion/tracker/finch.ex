defmodule Effusion.Tracker.Finch do
  alias Effusion.Tracker.Response
  @behaviour Effusion.Tracker.HTTP

  def announce(request) do
    query =
      Map.from_struct(request)
      |> Map.drop([:url])
      |> Enum.reject(fn {_key, val} -> is_nil(val) end)
      |> URI.encode_query()

    request = Finch.build(:get, request.url <> "?" <> query)

    with {:ok, response} when response.status == 200 <- Finch.request(request, EffusionFinch),
         {:ok, bterm} <- Bento.decode(response.body) do
      if not is_nil(bterm["failure reason"]) do
        {:error, bterm["failure reason"]}
      else
        response = Response.decode(bterm)
        {:ok, response}
      end
      else
        {:ok, %Finch.Response{}} -> {:error, :tracker_http_error}
      end
  end
end
