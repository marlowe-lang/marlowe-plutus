import type { BlockHash, BlockNo, SlotNo, TxIdHex } from "./cardano.js";
import * as pg from 'pg';
import * as db from "./marloweIndexer/db.js";
import { waitFor } from "./async.js";
import { TxOutRefRecord } from "./cardano.js";

const DEFAULT_DB_CONFIG: pg.ClientConfig = {
  host: '127.0.0.1',
  port: 15432,
  database: 'marlowe',
  user: 'marlowe'
};

export const TEN_SECS_IN_MS = 10000;

// DB:
// export interface CreateTxOutRecord {
//   txid: Buffer;
//   txix: number;
//   blockid: Buffer;
//   metadata: Buffer | null;
//   slotno: bigint;
//   blockno: bigint;
// }

export interface CreateTxOut {
  txOutRef: TxOutRefRecord;
  // TODO: Decode metadata
  metadata: Buffer | null;
  slotNo: SlotNo;
  blockNo: BlockNo;
  blockHash: BlockHash;
}

namespace CreateTxOut {
  export const fromDbCreateTxOutRecord = (rec: db.CreateTxOutRecord): CreateTxOut => {
    return {
      txOutRef: {
        txId: rec.txid.toString('hex') as TxIdHex,
        index: rec.txix,
      },
      metadata: rec.metadata,
      blockHash: rec.blockid as BlockHash,
      slotNo: rec.slotno as SlotNo,
      blockNo: rec.blockno as BlockNo,
    }
  }
}

export async function waitForCreateTxOuts(
  txId: TxIdHex,
  timeoutMs: number = TEN_SECS_IN_MS,
  dbClient: pg.Client | null = null,
): Promise<CreateTxOut[]> {
  const doDisconnect = dbClient === null;
  if(!dbClient) {
    dbClient = new pg.Client(DEFAULT_DB_CONFIG);
    await dbClient.connect();
  }
  try {
    // This could be easily optimized by moving the
    // filtering to the database query. For now it is
    // sufficient.
    const createTxOuts = await waitFor(
      () => db.queryCreateTxOut(dbClient),
      (creteTxOuts: db.CreateTxOutRecord[]) => creteTxOuts.some(txout => db.txIdToHex(txout.txid) === txId),
      { timeoutMs: timeoutMs }
    );
    const matching = createTxOuts.filter((txout: db.CreateTxOutRecord) => db.txIdToHex(txout.txid) === txId);
    return matching.map(CreateTxOut.fromDbCreateTxOutRecord);
  } finally {
    if(doDisconnect && dbClient) {
      await dbClient.end();
    }
  }
}

export async function waitForCreateTxOut(
  txId: TxIdHex,
  timeoutMs: number = TEN_SECS_IN_MS,
  dbClient: pg.Client | null = null,
): Promise<CreateTxOut> {
  const createTxOuts = await waitForCreateTxOuts(txId, timeoutMs, dbClient);
  if(createTxOuts.length === 0) {
    throw new Error(`No CreateTxOut found for txId ${txId}`);
  }
  return createTxOuts[0];
}


// DB:
// export interface ApplyTxRecord {
//   txid: Buffer;
//   outputtxix: number | null;
//
//
//   createtxid: Buffer;
//   createtxix: number;
//
//   blockid: Buffer;
//   invalidbefore: Date;
//   invalidhereafter: Date;
//   metadata: Buffer | null;
//
//   inputtxid: Buffer;
//   inputtxix: number;
//
//   inputs: Buffer;
//
//   slotno: bigint;
//   blockno: bigint;
// }

export type ApplyTx = {
  // Contract output:
  txOutRef: TxOutRefRecord;
  createTxOutRef: TxOutRefRecord;
  blockHash: BlockHash;
  invalidBefore: Date;
  invalidHereafter: Date;
  metadata: Buffer | null;
  // Contract input:
  inputTxOutRef: TxOutRefRecord;
};

namespace ApplyTx {
  export const fromDbApplyTxRecord = (rec: db.ApplyTx): ApplyTx => {
    return {
      txOutRef: {
        txId: rec.txid.toString('hex') as TxIdHex,
        index: rec.outputtxix === null ? -1 : rec.outputtxix,
      },
      createTxOutRef: {
        txId: rec.createtxid.toString('hex') as TxIdHex,
        index: rec.createtxix,
      },
      blockHash: rec.blockid as BlockHash,
      invalidBefore: rec.invalidbefore,
      invalidHereafter: rec.invalidhereafter,
      metadata: rec.metadata,
      inputTxOutRef: {
        txId: rec.inputtxid.toString('hex') as TxIdHex,
        index: rec.inputtxix,
      }
    }
  }
}

export async function waitForApplyTx(
  applyTxId: TxIdHex,
  timeoutMs: number = TEN_SECS_IN_MS,
  dbClient: pg.Client | null = null,
): Promise<ApplyTx> {
  const doDisconnect = dbClient === null;
  if(!dbClient) {
    dbClient = new pg.Client(DEFAULT_DB_CONFIG);
    await dbClient.connect();
  }
  try {
    const applyTxs = await waitFor(
      () => db.queryApplyTx(dbClient),
      (applyTxs: db.ApplyTx[]) => applyTxs.some(tx => {
        const txIdHex = db.txIdToHex(tx.txid);
        return txIdHex === applyTxId;
      }),
      { timeoutMs: timeoutMs }
    );
    const matching = applyTxs.filter(tx => {
      const txIdHex = db.txIdToHex(tx.txid);
      return txIdHex === applyTxId;
    });
    if(matching.length === 0) {
      throw new Error(`No ApplyTx found for txId ${applyTxId}`);
    }
    return ApplyTx.fromDbApplyTxRecord(matching[0]);
  } finally {
    if(doDisconnect && dbClient) {
      await dbClient.end();
    }
  }
}

