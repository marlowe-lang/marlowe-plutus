import { err, ok, Result } from "neverthrow";
import type { Tagged } from "type-fest";

// Type level tagging of the types.
// Please note that this is somewhat weak type safety
// as it is enough to say: `"x" as TxIdHex` to convince
// the compiler that x is of type `TxIdHex`.
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

// TODO: Migrate to bigint
export type Lovelace = number;
export const ONE_ADA: Lovelace = 1_000_000;

export type AddressBech32 = Tagged<string, "AddressBech32">;
export type TestnetMagic = Tagged<number, "TestnetMagic">;
export type BlockNo = Tagged<bigint, "BlockNo">;
export type BlockHash = Tagged<Buffer, "BlockHash">;
export type SlotNo = Tagged<bigint, "SlotNo">;
