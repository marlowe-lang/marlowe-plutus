import * as pg from 'pg';
import * as db from "./marloweIndexer/db.js";
import { waitFor } from "./async.js";
import type { BlockHash, BlockNo, SlotNo, TxId, TxIdHex, TxIx, TxOutRef } from "@konduit/konduit-consumer/cardano";

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
//   txix: bigint;
//   blockid: Buffer;
//   metadata: Buffer | null;
//   slotno: bigint;
//   blockno: bigint;
// }

export interface CreateTxOut {
  txOutRef: TxOutRef;
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
        txId: rec.txid as Uint8Array as TxId,
        txIx: rec.txix as TxIx,
      },
      metadata: rec.metadata,
      blockHash: rec.blockid as Uint8Array as BlockHash,
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
  txOutRef: TxOutRef | null;
  createTxOutRef: TxOutRef;
  blockHash: BlockHash;
  invalidBefore: Date;
  invalidHereafter: Date;
  metadata: Buffer | null;
  // Contract input:
  inputTxOutRef: TxOutRef;
};

namespace ApplyTx {
  export const fromDbApplyTxRecord = (rec: db.ApplyTx): ApplyTx => {
    const txOutRef: TxOutRef | null = (() => {
      if(rec.outputtxix === null) {
        return null;
      }
      return {
        txId: rec.txid as Uint8Array as TxId,
        txIx: rec.outputtxix as TxIx
      };
    })();
    return {
      txOutRef,
      createTxOutRef: {
        txId: rec.createtxid as Uint8Array as TxId,
        txIx: rec.createtxix as TxIx
      },
      blockHash: rec.blockid as Uint8Array as BlockHash,
      invalidBefore: rec.invalidbefore,
      invalidHereafter: rec.invalidhereafter,
      metadata: rec.metadata,
      inputTxOutRef: {
        txId: rec.inputtxid as Uint8Array as TxId,
        txIx: rec.inputtxix as TxIx
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

