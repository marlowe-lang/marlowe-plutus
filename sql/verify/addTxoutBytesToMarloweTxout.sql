BEGIN;
DO $$
BEGIN
  IF EXISTS (
      SELECT 1
      FROM information_schema.columns
      WHERE table_schema = 'marlowe'
        AND table_name = 'txOut'
        AND column_name = 'someTransactionScriptOutput'
  ) THEN
      RAISE EXCEPTION 'Verification failed: column marlowe.txOut.someTransactionScriptOutput BYTEA NOT NULL is missing or has wrong definition';
  END IF;
END $$;
ROLLBACK;
