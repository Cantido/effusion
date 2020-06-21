alias Effusion.DHT.Bucket
alias Effusion.Repo

unless Repo.exists?(Bucket) do
  Bucket.seed_bucket()
  |> Repo.insert()
end
