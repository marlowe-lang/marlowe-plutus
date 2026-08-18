BEGIN;

CREATE TYPE indexer_status_attr AS ENUM ('tip');

CREATE TABLE IF NOT EXISTS indexer_status (
    attr indexer_status_attr PRIMARY KEY,
    value bytea
);

COMMIT;
