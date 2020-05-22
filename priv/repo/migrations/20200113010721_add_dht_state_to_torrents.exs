defmodule Effusion.Repo.Migrations.AddDhtStateToTorrents do
  use Ecto.Migration

  # we have to dance around with the defaults and with deleting/creating the torrent_status
  # because altering the enum is not allowed in a transaction.
  def up do
    execute "ALTER TABLE torrents ALTER COLUMN state TYPE VARCHAR(255);"
    execute "ALTER TABLE torrents ALTER COLUMN state SET DEFAULT 'paused';"
    execute "drop type if exists torrent_status;"
    execute "create type torrent_status as enum ('paused', 'downloading', 'finished', 'dht_only');"
    execute "ALTER TABLE torrents ALTER COLUMN state SET DEFAULT 'paused'::torrent_status;"
    execute "ALTER TABLE torrents ALTER COLUMN state TYPE torrent_status USING (state::torrent_status);"
  end

  def down do
    execute "ALTER TABLE torrents ALTER COLUMN state TYPE VARCHAR(255);"
    execute "ALTER TABLE torrents ALTER COLUMN state SET DEFAULT '';"
    execute "drop type if exists torrent_status;"
    execute "create type torrent_status as enum ('paused', 'downloading', 'finished');"
    execute "ALTER TABLE torrents ALTER COLUMN state SET DEFAULT 'paused'::torrent_status;"
    execute "ALTER TABLE torrents ALTER COLUMN state TYPE torrent_status USING (state::torrent_status);"
  end
end
