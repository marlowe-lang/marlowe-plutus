BEGIN;

CREATE TYPE node_status_attr AS ENUM ('tip');

CREATE TABLE IF NOT EXISTS node_status (
    attr node_status_attr PRIMARY KEY,
    value bytea
);

COMMIT;
