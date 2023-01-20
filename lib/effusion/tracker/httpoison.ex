defmodule Effusion.Tracker.HTTPoison do
  alias Effusion.Tracker.Response
  require Logger
  @behaviour Effusion.Tracker.HTTP

  def announce(request) do
    query =
      Map.from_struct(request)
      |> Map.drop([:url])
      |> Map.update!(:ip, &:inet.ntoa/1)
      |> Map.update!(:ip, &to_string/1)
      |> Enum.reject(fn {_key, val} -> is_nil(val) end)
      |> URI.encode_query()

    with {:ok, %{status: 200, body: body}} <- HTTPoison.get(request.url <> "?" <> query, [], timeout: 30_000, recv_timeout: 30_000),
         {:ok, bterm} <- Bento.decode(body) do
      if not is_nil(bterm["failure reason"]) do
        {:error, bterm["failure reason"]}
      else
        response = Response.decode(bterm)
        {:ok, response}
      end
    else
      {:ok, _} -> {:error, :tracker_http_error}
    end
  end
end
