BEGIN;

DO $$
BEGIN
  -- Check if type exists
  IF NOT EXISTS (
    SELECT 1 FROM pg_type WHERE typname = 'indexer_status_attr'
  ) THEN
    RAISE EXCEPTION 'Type "indexer_status_attr" does not exist';
  END IF;

  IF NOT EXISTS (
    SELECT 1 FROM information_schema.tables WHERE table_name = 'indexer_status'
  ) THEN
    RAISE EXCEPTION 'Table "indexer_status" does not exist';
  END IF;
END;
$$;

ROLLBACK;
