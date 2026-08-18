import { runCommand, runCommandJson, type CommandError } from './exec.js';
import * as nodeOs from "node:os";
import * as nodeFs from "node:fs";
import {
  type AddressBech32,
  type TestnetMagic,
  type TxHex,
  type TxIdHex,
  TxOutRefHex,
} from "./cardano.js";
import { err, ok, ResultAsync, type Result } from 'neverthrow';
import * as jsonCodecs from '@konduit/codec/json/codecs';
import { fromSafePromiseResultFn, unwrapOrPanicWith, toAsync } from '@konduit/konduit-consumer/neverthrow';
import type { Milliseconds } from '@konduit/konduit-consumer/time/duration';
import type { Tagged } from 'type-fest';
import { TxEnvelope } from '@marlowe-lang/runtime/client';

// Types which are not Cardano generic but a rather used in the context of the cardano-cli
// should be defined here.

export type Path = string;

export type Tip = {
  block: number;
  epoch: number;
  era: string;
  hash: string;
  slot: number;
  slotInEpoch: number;
  slotsToEpochEnd: number;
  syncProgress: number;
};

export type TxOut = {
  address: AddressBech32;
  datum: string | null;
  datumhash: string | null;
  inlineDatum: string | null;
  inlineDatumRaw: string | null;
  referenceScript: string | null;
  value: Record<string, Record<string, number>>;
};

export type UtxoMap = {
  [txOutRef: TxOutRefHex]: TxOut;
};

export namespace UtxoMap {
  export const toUtxos = (utxoMap: UtxoMap): Utxo[] => {
    return Object.entries(utxoMap).map(([txOutRefHex, txOut]) => {
      return {
        txOutRef: txOutRefHex as TxOutRefHex,
        txOut,
      };
    });
  }
}

export type Utxo = { txOutRef: TxOutRefHex; txOut: TxOut };

export const getTip = (): Tip => {
  const tip: Tip = unwrapOrPanicWith(runCommandJson(`cardano-cli query tip`), (err) => `Failed to get tip: ${err}`);
  const syncProgress = typeof tip.syncProgress === 'string' ? parseFloat(tip.syncProgress) : tip.syncProgress;
  if (syncProgress < 1) {
    throw new Error(`Node is not fully synchronized (${syncProgress}%)`);
  }
  return tip;
};

export const queryUtxoByAddress = (address: AddressBech32, testnetMagic: TestnetMagic): Result<UtxoMap, CommandError | string> => {
  return runCommandJson(
    `cardano-cli query utxo --testnet-magic ${testnetMagic} --address "${address}" --output-json`
  );
};

export const getTxIdFromTxFile = (txFilePath: Path): Result<TxIdHex, CommandError | string> => {
  return runCommandJson(
    `cardano-cli conway transaction txid --tx-file ${txFilePath}`,
  ).map((res: any) => res.txhash as TxIdHex);
};

export const getTxIdFromTxBodyFile = (txBodyFilePath: Path): Result<TxIdHex, CommandError | string> => {
  return runCommandJson(
    `cardano-cli conway transaction txid --tx-body-file ${txBodyFilePath}`,
  ).map((res: any) => res.txhash as TxIdHex)
};

export const getTxId = (txHex: TxHex, tmpDir: Path | null = null): Result<TxIdHex, CommandError | string> => {
  const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txTmpDir = `${rootTmpDir}/txid-${Date.now()}-${Math.floor(Math.random() * 1000000)}`;
  nodeFs.mkdirSync(txTmpDir, { recursive: true });
  const txFilePath = `${txTmpDir}/tx.json`;
  const txJson = {
    type: "Tx ConwayEra",
    description: "Ledger Cddl Format",
    cborHex: txHex,
  };
  nodeFs.writeFileSync(txFilePath, JSON.stringify(txJson));
  return getTxIdFromTxFile(txFilePath);
};

export type TimeoutError = {
  type: "TimeoutError";
  message: string;
  timeout: Milliseconds;
};

export type WaitForUtxoError = CommandError | string | TimeoutError;

export const waitForUtxo: (txOutRef: TxOutRefHex, delayMs?: number, timeoutMs?: number) => ResultAsync<TxOut, WaitForUtxoError> = fromSafePromiseResultFn(async (txOutRef, delayMs = 1000, timeoutMs = 200_000,) => {
  const startTime = Date.now();
  const loop = async (): Promise<Result<TxOut, WaitForUtxoError>> => {
    if(Date.now() - startTime < timeoutMs) {
      return runCommandJson(
        `cardano-cli conway query utxo --tx-in "${txOutRef}" --output-json`,
      ).match(
        async (utxos) => {
          if (typeof utxos === 'object' && utxos !== null && Object.keys(utxos).length === 1) {
            const utxo = Object.values(utxos)[0];
            return ok(utxo as TxOut);
          }
          await new Promise(resolve => setTimeout(resolve, delayMs));
          return loop();
        },
        async e => err(e)
      );
    }
    return err({
      type: "TimeoutError",
      message: `Utxo ${txOutRef} did not appear within ${timeoutMs}ms`,
      timeout: timeoutMs,
    } as TimeoutError);
  }
  return loop();
});

// This function assumes that there is no other consumer of a given
// transaction output #0 as it awaits for that particular UTxO.
export const waitForTx = (
  txId: TxIdHex,
  delayMs = 1000,
  timeoutMs = 200_000,
): ResultAsync<null, WaitForUtxoError> => {
  const txOutRef = TxOutRefHex.fromTxOutRefRecord({ txId, index: 0 });
  return waitForUtxo(txOutRef, delayMs, timeoutMs).map(() => null);
};

export const signTxEnvelope = function (
  skeyFile: string,
  txEnvelope: TxEnvelope,
  tmpDir: Path | null = null, // When null we will use system temp dir
  debug = true,
): Result<TxEnvelope, jsonCodecs.JsonError | CommandError | string> {
  const txTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txEnvelopeJson = jsonCodecs.mkStringifier(TxEnvelope.jsonCodec)(txEnvelope);
  const unsignedTxFile = `${txTmpDir}/unsigned.tx.json`;
  const signedTxFile = `${txTmpDir}/sined.tx.json`;
  nodeFs.writeFileSync(unsignedTxFile, txEnvelopeJson);
  return runCommand(`cardano-cli conway transaction sign --tx-file ${unsignedTxFile} --signing-key-file ${skeyFile} --out-file ${signedTxFile}`, debug)
    .andThen(() => {
      const signedTxJsonStr = nodeFs.readFileSync(signedTxFile, "utf8");
      return jsonCodecs.mkParser(TxEnvelope.jsonCodec)(signedTxJsonStr);
    });
};

export const signTx = function (
  skeyFile: string,
  txHex: TxHex,
  tmpDir: Path | null = null, // When null we will use system temp dir
): Result<TxHex, CommandError | string> {
  return getTxId(txHex).andThen(txIdHex => {
    // Create temp directory for transaction signing
    const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
    const txTmpDir = `${rootTmpDir}/tx-${txIdHex}`;
    nodeFs.mkdirSync(txTmpDir, { recursive: true });

    // Prepare transaction body for signing
    const txBodyJson = {
      type: "Unwitnessed Tx ConwayEra",
      description: "Ledger Cddl Format",
      cborHex: txHex,
    };
    const txBodyFile = `${txTmpDir}/tx.body.json`;
    const txSignedFile = `${txTmpDir}/tx.signed.json`;
    nodeFs.writeFileSync(txBodyFile, JSON.stringify(txBodyJson));

    // Sign the transaction
    return runCommand(
      `cardano-cli conway transaction sign --tx-body-file ${txBodyFile} --signing-key-file ${skeyFile} --out-file ${txSignedFile}`,
    ).map(() => {
      const signedTxJsonStr = nodeFs.readFileSync(txSignedFile, "utf8");
      const signedTxJson = JSON.parse(signedTxJsonStr);
      const signedTxHex: TxHex = signedTxJson.cborHex;
      return signedTxHex;
    });
  });
};

export type TxEnvelopeFile = Tagged<Path, 'TxEnvelopeFile'>;

export const submitTxFromEnvelopeFile = (
  txFile: Path,
  intervalMs = 1_000,
  timeoutMs = 180_000,
  debug = true,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  return toAsync(runCommand(`cardano-cli conway transaction submit --tx-file ${txFile}`, debug))
    .andThen(
      () => toAsync(getTxIdFromTxFile(txFile))
    ).andThen(
      (txId) => waitForTx( txId, intervalMs, timeoutMs).map(() => txId)
    );
};

export const submitTxEnvelope = (
  txEnvelope: TxEnvelope,
  tmpDir: Path | null = null,
  intervalMs = 1_000,
  timeoutMs = 180_000,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  const txEnvelopeJson = jsonCodecs.mkStringifier(TxEnvelope.jsonCodec)(txEnvelope);
  const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txTmpDir = `${rootTmpDir}/tx-submit-${Date.now()}-${Math.floor(Math.random() * 1000000)}`;
  nodeFs.mkdirSync(txTmpDir, { recursive: true });
  const txEnvelopeFile = `${txTmpDir}/tx.json`;
  nodeFs.writeFileSync(txEnvelopeFile, txEnvelopeJson);
  return submitTxFromEnvelopeFile(txEnvelopeFile, intervalMs, timeoutMs);
}

export type Era = "conway" | "babbage" | "dijkstra";

export const submitTx = (
  txHex: TxHex,
  era: Era = "conway",
  tmpDir: Path | null = null,
  intervalMs = 1_000,
  timeoutMs = 180_000,
  debug = true,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  return toAsync(getTxId(txHex))
    .andThen((txIdHex) => {
      // Create temp directory for transaction submission
      const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
      const txTmpDir = `${rootTmpDir}/tx-submit-${txIdHex}`;
      nodeFs.mkdirSync(txTmpDir, { recursive: true });
      const txTag = (() => {
        switch (era) {
          case "conway":
            return "Tx ConwayEra";
          case "babbage":
            return "Tx BabbageEra";
          case "dijkstra":
            return "Tx DijkstraEra";
        }
      })();
      // Prepare transaction body for submission
      const txEnvelope = {
        type: txTag,
        description: "Ledger Cddl Format",
        cborHex: txHex,
      };
      const txEnvelopeFile = `${txTmpDir}/tx.json`;
      nodeFs.writeFileSync(txEnvelopeFile, JSON.stringify(txEnvelope));
      return submitTxFromEnvelopeFile(txEnvelopeFile, intervalMs, timeoutMs, debug);
  });
}
