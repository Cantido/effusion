defmodule EffusionWeb.Resolvers.Sessions do
  alias Effusion.Statistics.{
    SessionDownloadAverage,
    SessionUploadAverage
  }
  def download_bytes_per_second(_root, _args, _info) do
    {:ok, SessionDownloadAverage.session_20sec_download_avg()}
  end

  def upload_bytes_per_second(_root, _args, _info) do
    {:ok, SessionUploadAverage.session_20sec_upload_avg()}
  end
end
