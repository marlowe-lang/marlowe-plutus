# Marlowe Indexer Database - AI generated summary

## Overview

PostgreSQL database (`marlowe` schema) used by the Marlowe Indexer to store Marlowe contract state, transactions, and payouts.

## Schema

### Core Tables

| Table                               | Description                            | Used by
|-------                              |-------------                           |------------
| `marlowe.block`                     | Blockchain blocks (rollback reference) |
| `marlowe.txOut`                     | Transaction outputs                    | contractTxOut, payoutTxOut, txOutAsset
| `marlowe.txOutAsset`                | Assets at transaction outputs          |
| `marlowe.contractTxOut`             | Marlowe contract outputs               |
| `marlowe.createTxOut`               | Contract creation outputs              |
| `marlowe.applyTx`                   | Contract application transactions      |
| `marlowe.payoutTxOut`               | Payout outputs for withdrawals         |
| `marlowe.withdrawalTxIn`            | Withdrawal inputs                      |
| `marlowe.invalidApplyTx`            | Failed contract applications           |
| `marlowe.rollbackBlock`             | Rollback block tracking                |
| `marlowe.contractTxOutTag`          | Contract tags                          |
| `marlowe.contractTxOutPartyAddress` | Party addresses                        |
| `marlowe.contractTxOutPartyRole`    | Party roles                            |

### Table Details

#### `marlowe.block`
```
- id            BYTEA (PK)      -- Block hash
- slotNo        BIGINT (NN)     -- Slot number
- blockNo       BIGINT (NN)     -- Block number
- rollbackToBlock BYTEA         -- Block to rollback to
- rollbackToSlot  BIGINT        -- Slot to rollback to
```
Indexes: BTREE on slotNo, BTREE on (slotNo, id)

#### `marlowe.txOut`
```
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)   -- Output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- address       BYTEA (NN)      -- Output address
- lovelace      BIGINT (NN)     -- ADA amount
```
PK: (txId, txIx)
Indexes: BTREE on blockId, BTREE on MD5(address)

#### `marlowe.txOutAsset`
```
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- policyId      BYTEA (NN)      -- Token policy ID
- name          BYTEA (NN)      -- Token name
- quantity      BIGINT (NN)     -- Token quantity
- FK: (txId, txIx) -> marlowe.txOut
```
PK: (txId, txIx, policyId, name)
Indexes: BTREE on blockId, BTREE on (txId, txIx)

#### `marlowe.contractTxOut`
```
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- payoutScriptHash BYTEA (NN)  -- Payout script hash
- contract      BYTEA (NN)     -- Contract state
- state         BYTEA (NN)     -- Contract state
- rolesCurrency BYTEA (NN)     -- Roles token currency
- FK: (txId, txIx) -> marlowe.txOut
```
PK: (txId, txIx)
Indexes: BTREE on blockId, BTREE on rolesCurrency

#### `marlowe.createTxOut`
```
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)   -- Output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- metadata      BYTEA           -- Creation metadata
- slotNo        BIGINT (NN)     -- Slot number
- blockNo       BIGINT (NN)     -- Block number
- FK: (txId, txIx) -> marlowe.contractTxOut
```
PK: (txId, txIx)
Indexes: BTREE on blockId, BTREE on slotNo, BTREE on (slotNo, txId, txIx)

#### `marlowe.applyTx`
```
- txId          BYTEA (PK)      -- Application transaction ID
- createTxId    BYTEA (NN)      -- Contract creation transaction ID
- createTxIx    SMALLINT (NN)   -- Contract creation output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- invalidBefore TIMESTAMP (NN)  -- Validity interval start
- invalidHereafter TIMESTAMP (NN) -- Validity interval end
- metadata      BYTEA           -- Application metadata
- inputTxId     BYTEA (NN)      -- Input contract transaction ID
- inputTxIx     SMALLINT (NN)  -- Input contract output index
- inputs        BYTEA (NN)     -- Contract inputs
- outputTxIx    SMALLINT        -- Output contract index
- slotNo        BIGINT (NN)    -- Slot number
- blockNo       BIGINT (NN)    -- Block number
- FK: (createTxId, createTxIx) -> marlowe.createTxOut
- FK: (txId, outputTxIx) -> marlowe.contractTxOut
- FK: (inputTxId, inputTxIx) -> marlowe.contractTxOut
```
Indexes: BTREE on blockId, BTREE on (createTxId, createTxIx), BTREE on (txId, outputTxIx), BTREE on (inputTxId, inputTxIx), BTREE on slotNo, BTREE on (slotNo, txId)

#### `marlowe.payoutTxOut`
```
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- rolesCurrency BYTEA (NN)     -- Roles currency
- role          BYTEA (NN)     -- Role name
- FK: (txId, txIx) -> marlowe.txOut
- FK: txId -> marlowe.applyTx
```
PK: (txId, txIx)
Indexes: BTREE on blockId, BTREE on txId, BTREE on rolesCurrency, BTREE on (rolesCurrency, role)

#### `marlowe.withdrawalTxIn`
```
- txId          BYTEA (NN)      -- Withdrawal transaction ID
- blockId       BYTEA (NN) (FK) -- Reference to block
- payoutTxId    BYTEA (NN)      -- Payout transaction ID
- payoutTxIx   SMALLINT (NN)   -- Payout output index
- createTxId    BYTEA (NN)      -- Contract creation transaction ID
- createTxIx    SMALLINT (NN)   -- Contract creation output index
- slotNo        BIGINT (NN)     -- Slot number
- blockNo       BIGINT (NN)     -- Block number
- FK: (payoutTxId, payoutTxIx) -> marlowe.payoutTxOut
- FK: (createTxId, createTxIx) -> marlowe.createTxOut
```
PK: (payoutTxId, payoutTxIx)
Indexes: BTREE on blockId, BTREE on txId, BTREE on (createTxId, createTxIx), BTREE on slotNo, BTREE on (slotNo, txId)

#### `marlowe.invalidApplyTx`
```
- txId          BYTEA (PK)      -- Failed application transaction ID
- inputTxId     BYTEA (NN)      -- Input contract transaction ID
- inputTxIx     SMALLINT (NN)  -- Input contract output index
- blockId       BYTEA (NN) (FK) -- Reference to block
- error         TEXT (NN)       -- Error message
- FK: (inputTxId, inputTxIx) -> marlowe.contractTxOut
```
Indexes: BTREE on blockId, BTREE on (inputTxId, inputTxIx)

#### `marlowe.rollbackBlock`
```
- fromBlock     BYTEA (PK)      -- Block to rollback from
- toBlock       BYTEA (NN) (FK) -- Block to rollback to
- toSlotNo      BIGINT (NN)    -- Target slot number
```
Indexes: BTREE on toSlotNo, BTREE on toBlock

#### `marlowe.contractTxOutTag`
```
- tag           TEXT (NN)       -- Tag (changed from VARCHAR(64))
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- FK: (txId, txIx) -> marlowe.contractTxOut ON DELETE CASCADE
```
PK: (tag, txId, txIx)
Indexes: BTREE on tag

#### `marlowe.contractTxOutPartyAddress`
```
- address       BYTEA (NN)      -- Party address
- txId          BYTEA (NN)      -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- createTxId    BYTEA (NN)      -- Contract creation transaction ID
- createTxIx    SMALLINT (NN)   -- Contract creation output index
- FK: (txId, txIx) -> marlowe.contractTxOut ON DELETE CASCADE
- FK: (createTxId, createTxIx) -> marlowe.createTxOut ON DELETE CASCADE
```
PK: (address, txId, txIx)
Indexes: BTREE on address

#### `marlowe.contractTxOutPartyRole`
```
- rolesCurrency BYTEA (NN)     -- Roles currency
- role          BYTEA (NN)      -- Role name
- txId          BYTEA (NN)     -- Transaction ID
- txIx          SMALLINT (NN)  -- Output index
- createTxId    BYTEA (NN)     -- Contract creation transaction ID
- createTxIx    SMALLINT (NN)  -- Contract creation output index
- FK: (txId, txIx) -> marlowe.contractTxOut ON DELETE CASCADE
- FK: (createTxId, createTxIx) -> marlowe.createTxOut ON DELETE CASCADE
```
PK: (rolesCurrency, role, txId, txIx)
Indexes: BTREE on (rolesCurrency, role)

## Sqitch Migration Plan

| Step | Name | Dependencies | Description |
|------|------|--------------|-------------|
| 1 | schema | - | Create marlowe schema and base tables |
| 2 | block-cols | schema | Add slotNo/blockNo columns to aggregate tables |
| 3 | rollback | schema | Move rollback info to separate table |
| 4 | withdrawalCreateNotNull | block-cols | Add NOT NULL constraints to withdrawal columns |
| 5 | fixPayouts | schema | Fix payout column swap (rolesCurrency/role) |
| 6 | indexRoleCurrency | schema | Add indexes on rolesCurrency columns |
| 7 | tags | schema | Add contract tags table |
| 8 | tag-text | tags | Change tag from VARCHAR(64) to TEXT |
| 9 | parties | schema | Add party indexes (address and role) |
| 10 | resetParties | parties | Truncate party tables |

## Key Design Decisions

1. **Separate rollback tracking**: `rollbackBlock` table tracks rollback information separately from block table
2. **Contract lineage tracking**: `createTxId/createTxIx` links applications to original contract creation
3. **Payout mechanism**: Separate `payoutTxOut` and `withdrawalTxIn` tables for the two-phase withdrawal process
4. **Party identification**: Separate tables for address-based and role-based parties
5. **CASCADE deletes**: Party and tag tables use CASCADE to maintain referential integrity
6. **Slot/block redundancy**: slotNo and blockNo stored on aggregate tables for query performance

## Database Connection

Target: `db:pg://postgres@0.0.0.0/chain`

## Extracted SQL Queries

SQL queries extracted from Haskell code in `marlowe-cardano/marlowe-runtime/indexer/`:

### Core Indexer Queries (`queries/`)

| Query | Description |
|-------|-------------|
| `commitBlocks.sql` | Bulk insert of Marlowe block data (blocks, txOuts, assets, contracts, applications, payouts, withdrawals, tags) |
| `commitRollback.sql` | Handle rollback to a specific chain point |
| `getMaxBlockNo.sql` | Get the maximum block number from marlowe.block |
| `getIntersectionPoints.sql` | Find chain intersection points for sync (requires security parameter offset) |
| `getMarloweUTxO_ContractOutputs.sql` | Query unspent Marlowe contract outputs (UTxO part 1) |
| `getMarloweUTxO_PayoutOutputs.sql` | Query unspent payout outputs (UTxO part 2) |

### Party Indexing Queries (`queries/`)

| Query | Description |
|-------|-------------|
| `commitParties.sql` | Bulk insert party addresses and roles |
| `loadContracts_FirstSlot.sql` | Find starting slot for party indexing (resuming) |
| `loadContracts.sql` | Load contracts for party indexing (batch of 16,384) |

### SQL to Haskell File Mapping

| SQL File | Haskell File | Description |
|----------|--------------|-------------|
| `commitBlocks.sql` | `CommitBlocks.hs` | Bulk insert of Marlowe block data |
| `commitRollback.sql` | `CommitRollback.hs` | Handle rollback to a specific chain point |
| `getMaxBlockNo.sql` | `GetIntersectionPoints.hs` | (inline in getIntersectionPoints query) |
| `getIntersectionPoints.sql` | `GetIntersectionPoints.hs` | Find chain intersection points for sync |
| `getMarloweUTxO_ContractOutputs.sql` | `GetMarloweUTxO.hs` | Query unspent Marlowe contract outputs |
| `getMarloweUTxO_PayoutOutputs.sql` | `GetMarloweUTxO.hs` | Query unspent payout outputs |
| `commitParties.sql` | `Party.hs` | Bulk insert party addresses and roles |
| `loadContracts.sql` | `Party.hs` | Load contracts for party indexing |
| `loadContracts_FirstSlot.sql` | `Party.hs` | Find starting slot for party indexing |

### Additional Haskell Source Files

| File | Description |
|------|-------------|
| `PostgreSQL.hs` | Database wiring module - exports `databaseQueries` |
| `Database.hs` | Database interface - defines `DatabaseQueries` type |
| `Types.hs` | MarloweBlock, MarloweTransaction, MarloweUTxO types |
| `Indexer.hs` | Main indexer logic |
| `Store.hs` | Indexer store operations |
| `ChainSeekClient.hs` | ChainSeek client integration |

### Source Location

All Haskell files copied from:
`marlowe-cardano/marlowe-runtime/indexer/Language/Marlowe/Runtime/Indexer/`

### Database Interface

The indexer exposes these operations via `DatabaseQueries`:

```haskell
data DatabaseQueries m = DatabaseQueries
  { commitRollback       :: ChainPoint -> m ()
  , commitBlocks        :: [MarloweBlock] -> m ()
  , getIntersectionPoints :: m [BlockHeader]
  , getMarloweUTxO      :: BlockHeader -> m MarloweUTxO
  }
```

### Note on Marlowe Sync Queries

The `marlowe-sync` component (`marlowe-runtime/sync/`) also uses the `marlowe` schema for reading data to serve queries. These queries are separate and located in:

- `marlowe-cardano/marlowe-runtime/sync/Language/Marlowe/Runtime/Sync/Database/PostgreSQL/`

The sync queries include: `GetContractState`, `GetCreateStep`, `GetHeaders`, `GetIntersection`, `GetNextBlocks`, `GetTransaction`, `GetPayout`, `GetWithdrawals`, etc.
