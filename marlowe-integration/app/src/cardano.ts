import { err, ok, Result } from "neverthrow";
import type { Tagged } from "type-fest";

// TODO: A lot of this module should be at unified with
// our growing cardano-ts library which is currently
// linked through a portal locally.

// We use type level tagging across the project.
// Please avoid using `as` casting at all cost.
// Types which are tagged carry with them usually some
// invariants enforced either by the "smart contructor"
// or by the source of the data.
export type TxBodyHex = Tagged<string, "TxBodyHex">;
export type TxHex = Tagged<string, "TxHex">;
export type WitnessSetHex = Tagged<string, "WitnessSetHex">;
export type TxIdHex = Tagged<string, "TxIdHex">;
export type TxOutRefRecord = {
  txId: TxIdHex;
  index: number;
};
// Also called tx-in
export namespace TxOutRefRecord {
  export const fromTxOutRefHex = (txOutRefHexStr: TxOutRefHex): TxOutRefRecord => {
    const [txId, indexStr] = txOutRefHexStr.split("#");
    return {
      txId: txId as TxIdHex,
      index: parseInt(indexStr, 10),
    };
  }

  export const areEqual = (a: TxOutRefRecord, b: TxOutRefRecord): boolean => {
    return a.txId === b.txId && a.index === b.index;
  }
}

// Concatenated txId#txIx
export type TxOutRefHex = Tagged<string, "TxOutRefHex">;
export namespace TxOutRefHex {
  export const fromString = (str: string): Result<TxOutRefHex, string> => {
    const [txId, indexStr] = str.split("#");
    if (!txId || !indexStr) {
      return err(`Invalid TxOutRefHexStr: ${str}`);
    }
    if (!/^[0-9a-fA-F]+$/.test(txId) || txId.length !== 64) {
      return err(`Invalid TxIdHex in TxOutRefHexStr: ${txId}`);
    }
    if (!/^\d+$/.test(indexStr)) {
      return err(`Invalid index in TxOutRefHexStr: ${indexStr}`);
    }
    return ok(str as TxOutRefHex);
  }

  export const fromTxOutRefRecord = (txOutRef: TxOutRefRecord): TxOutRefHex => {
    return `${txOutRef.txId}#${txOutRef.index}` as TxOutRefHex;
  }
}

export type Lovelace = number;
export const ONE_ADA: Lovelace = 1_000_000;

export type AddressBech32 = Tagged<string, "AddressBech32">;
export type TestnetMagic = Tagged<number, "TestnetMagic">;
export type BlockNo = Tagged<bigint, "BlockNo">;
export type BlockHash = Tagged<Buffer, "BlockHash">;
export type SlotNo = Tagged<bigint, "SlotNo">;
