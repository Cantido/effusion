defmodule Effusion.Repo.Migrations.ConstrainBucketSize do
  use Ecto.Migration

  def up do
    execute """
    CREATE OR REPLACE FUNCTION enforce_bucket_size() RETURNS trigger AS $$
    DECLARE
        max_bucket_size INTEGER := 8;
        bucket_size INTEGER := 0;
        must_check BOOLEAN := false;
    BEGIN
        IF TG_OP = 'INSERT' THEN
            must_check := true;
        END IF;

        IF TG_OP = 'UPDATE' THEN
            IF (NEW.bucket_id != OLD.bucket_id) THEN
                must_check := true;
            END IF;
        END IF;

        IF must_check THEN
            -- prevent concurrent inserts from multiple transactions
            LOCK TABLE nodes IN EXCLUSIVE MODE;

            SELECT INTO bucket_size COUNT(*)
            FROM nodes
            WHERE bucket_id = NEW.bucket_id;

            IF bucket_size >= max_bucket_size THEN
                RAISE EXCEPTION 'Cannot insert more than % nodes for each bucket.', max_bucket_size;
            END IF;
        END IF;

        RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE TRIGGER enforce_bucket_size
        BEFORE INSERT OR UPDATE ON nodes
        FOR EACH ROW EXECUTE PROCEDURE enforce_bucket_size();
    """
  end

  def down do
    execute """
    DROP TRIGGER IF EXISTS enforce_bucket_size ON nodes CASCADE;
    """

    execute """
    DROP FUNCTION IF EXISTS enforce_bucket_size CASCADE;
    """
  end
end
