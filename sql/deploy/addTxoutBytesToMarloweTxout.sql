BEGIN;

ALTER TABLE marlowe.txOut
  ADD COLUMN someTransactionScriptOutput BYTEA NOT NULL;

COMMIT;
