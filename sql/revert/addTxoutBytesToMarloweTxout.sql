BEGIN;

ALTER TABLE marlowe.txOut
  DROP COLUMN someTransactionScriptOutput;

COMMIT;
