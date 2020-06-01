defmodule Effusion.Repo.Migrations.MakeNodeIdsNumeric do
  use Ecto.Migration

  # The magic number 1461501637330902918203684832716283019655932542976 is just 2^160
  # note that numrange(a, b) makes b an EXCLUSIVE boundary.
  # We must make boundaries exclusive so that they can overlap, since numrange is technically a floating-point range

  def up do
    alter table(:nodes) do
      remove :node_id
      add :node_id, :"numeric(49)", null: true
    end
    create constraint(:nodes, :node_id_must_be_within_160_bits, check: "node_id <= 1461501637330902918203684832716283019655932542976")
    create constraint(:nodes, :node_id_must_be_non_negative, check: "node_id >= 0")

    alter table(:buckets) do
      remove :minimum
      remove :maximum

      add :range, :numrange, null: false
    end

    create constraint(:buckets, :must_be_within_range, check: "range <@ numrange(0,1461501637330902918203684832716283019655932542977)")

    execute """
    CREATE FUNCTION numrange_accum_sfunc(numrange, numrange)
    RETURNS numrange AS
    $$
            SELECT $1 + $2;
    $$ LANGUAGE 'sql' STRICT;
    """

    execute """
    CREATE AGGREGATE numrange_accum(numrange)
    (
      STYPE = numrange,
      SFUNC = numrange_accum_sfunc
    );
    """

    # makes sure that buckets do not overlap
    execute """
      alter table buckets
      add constraint buckets_do_not_overlap
      exclude using gist (range with &&)
      DEFERRABLE;
    """

    # makes sure that buckets completely cover [0,2^160]
    execute """
    CREATE OR REPLACE FUNCTION enforce_bucket_coverage() RETURNS trigger AS $$
    DECLARE
        bucket_range numrange := numrange(0,0);
    BEGIN
        LOCK TABLE buckets IN EXCLUSIVE MODE;

        SELECT INTO bucket_range numrange_accum(range)
        FROM buckets;

        IF bucket_range <> numrange(0,1461501637330902918203684832716283019655932542977) THEN
            RAISE EXCEPTION 'Bucket ranges must completely cover the range from 0 to 2^160 (inclusive). Coverage after inserting was %', bucket_range;
        END IF;

        RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE CONSTRAINT TRIGGER enforce_bucket_coverage
        AFTER INSERT OR UPDATE ON buckets
        DEFERRABLE
        FOR EACH ROW EXECUTE PROCEDURE enforce_bucket_coverage();
    """

    execute """
    CREATE OR REPLACE FUNCTION nodes_must_be_in_buckets() RETURNS trigger AS $$
    DECLARE
      target_bucket buckets%ROWTYPE := NULL;
    BEGIN
        SELECT *
        INTO target_bucket
        FROM buckets
        WHERE id = NEW.bucket_id;

        IF target_bucket IS NULL OR NOT target_bucket.range @> NEW.node_id THEN
            RAISE EXCEPTION 'Node must be inserted into a bucket that contains its node ID. Node ID was % but target bucket range was %', NEW.node_id, target_bucket.range;
        END IF;

        RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
    """

    execute """
    CREATE CONSTRAINT TRIGGER nodes_must_be_in_buckets
        AFTER INSERT OR UPDATE ON nodes
        FOR EACH ROW EXECUTE PROCEDURE nodes_must_be_in_buckets();
    """

    execute """
    CREATE OR REPLACE FUNCTION split_bucket(numeric) RETURNS void AS $$
    DECLARE
        range_to_split numrange := numrange(0,0);
        new_range_lower numrange := numrange(0,0);
        new_range_upper numrange := numrange(0,0);
        lower_bound numeric(49) := 0;
        middle_bound numeric(49) := 0;
        upper_bound numeric(49) := 0;
        lower_bucket bigint := 0;
        upper_bucket bigint := 0;
    BEGIN
        LOCK TABLE buckets IN EXCLUSIVE MODE;
        SET CONSTRAINTS enforce_bucket_coverage DEFERRED;
        SET CONSTRAINTS buckets_do_not_overlap DEFERRED;

        SELECT range
        INTO range_to_split
        FROM buckets
        WHERE range @> $1;

        IF range_to_split IS NULL THEN
          RAISE 'No range exists for node_id %', $1;
        END IF;

        lower_bound = lower(range_to_split);
        middle_bound = div(upper(range_to_split), 2);
        upper_bound = upper(range_to_split);

        new_range_lower = numrange(lower_bound, middle_bound);
        new_range_upper = numrange(middle_bound, upper_bound);

        INSERT INTO buckets(range) VALUES (new_range_lower) RETURNING id INTO lower_bucket;
        INSERT INTO buckets(range) VALUES (new_range_upper) RETURNING id INTO upper_bucket;

        UPDATE nodes SET bucket_id = lower_bucket
        WHERE node_id < middle_bound;

        UPDATE nodes SET bucket_id = upper_bucket
        WHERE node_id >= middle_bound;

        DELETE FROM buckets
        WHERE range = range_to_split;

        SET CONSTRAINTS enforce_bucket_coverage IMMEDIATE;
        SET CONSTRAINTS buckets_do_not_overlap IMMEDIATE;
    END;
    $$ LANGUAGE plpgsql;
    """
  end

  def down do
    execute "DROP AGGREGATE numrange_accum(numrange);"
    execute "DROP FUNCTION numrange_accum_sfunc CASCADE;"
    execute "DROP TRIGGER enforce_bucket_coverage ON buckets CASCADE;"
    execute "DROP FUNCTION enforce_bucket_coverage CASCADE;"
    execute "DROP TYPE numrange;"

    alter table(:nodes) do
      remove :node_id
      add :node_id, :binary
    end

    alter table(:buckets) do
      remove :range
      add :minimum, :binary
      add :maximum, :binary
    end
  end
end
