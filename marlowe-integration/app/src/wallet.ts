// WARNING: Fully AI generated. Not tested and not reviewed yet.
import { err, ok, type Result } from "neverthrow";
import * as nodeFs from "node:fs";
import * as nodePath from "node:path";
import {
  Wallet,
  type WalletBackendBase,
} from "@konduit/konduit-consumer/wallets/embedded";
import {
  Address,
  AddressBech32,
  Lovelace,
  Network,
  NetworkMagicNumber,
  PubKeyHash,
  TransactionUnspentOutput,
  TxHash,
  TxInput,
  TxIx,
  TxOut,
} from "@konduit/konduit-consumer/cardano";
import { PositiveBigInt } from "@konduit/codec/integers/big";
import { NonNegativeInt } from "@konduit/codec/integers/smallish";
import type { Ed25519PrivateKey } from "@konduit/cardano-keys/rfc8032";
import type { Ed25519VerificationKey } from "@konduit/cardano-keys";
import type { Transaction } from "@konduit/konduit-consumer/txBuilder";
import type {
  HttpEndpointError,
  NetworkError,
} from "@konduit/konduit-consumer/http";
import {
  runCommand,
  type Path,
} from "./exec.js";
import {
  queryUtxosByAddress,
  submitTxFromEnvelopeFile,
  type TxOut as AppTxOut,
  type UtxoMap,
} from "./cardanoCli.js";
import type { JsonError } from "@konduit/codec/json/codecs";

const resolveNetworkMagicFromEnv = (): Result<NetworkMagicNumber, JsonError> => {
  const raw = process.env["CARDANO_NODE_NETWORK_ID"];
  if (raw === undefined || raw === "") {
    return ok(NetworkMagicNumber.MAINNET);
  }
  return PositiveBigInt.fromString(raw).map(NetworkMagicNumber.fromPositiveBigInt);
};

const requireCardanoSocketPath = (): Result<Path, string> => {
  const socketPath = process.env["CARDANO_NODE_SOCKET_PATH"];
  if (!socketPath) {
    return err(
      "CARDANO_NODE_SOCKET_PATH environment variable must be set to use CardanoCliWallet",
    );
  }
  return ok(socketPath as Path);
};

const cliErrorToHttp = (e: unknown): HttpEndpointError => {
  const message = typeof e === "string" ? e : JSON.stringify(e);
  const error: NetworkError = {
    type: "NetworkError",
    message,
    requestInfo: {
      method: "cardano-cli",
      url: "n/a",
      headers: undefined,
      payload: undefined,
    },
  };
  return error;
};

// ---------------------------------------------------------------------------
// cardano-cli skey file emission
// ---------------------------------------------------------------------------

// cardano-cli's `.skey` JSON layout for an Ed25519 payment key:
//   { "type": "PaymentSigningKeyShelley_ed25519",
//     "description": "Payment Signing Key",
//     "cborHex": "5820" + <64 hex chars> }
// where `0x5820` is a CBOR byte string of length 32.
const SKEY_FILENAME = "payment.skey";
const SKEY_TYPE = "PaymentSigningKeyShelley_ed25519";
const CBOR_BYTE_STRING_32 = "5820";

const bytesToHex = (bytes: Uint8Array): string => {
  let out = "";
  for (let i = 0; i < bytes.length; i++) {
    out += (bytes[i] ?? 0).toString(16).padStart(2, "0");
  }
  return out;
};

const writeSkeyFile = (walletDir: string, privateKey: Ed25519PrivateKey): string => {
  const skeyPath = nodePath.join(walletDir, SKEY_FILENAME);
  const secretBytes = privateKey.secret as unknown as Uint8Array;
  const cborHex = CBOR_BYTE_STRING_32 + bytesToHex(secretBytes);
  nodeFs.writeFileSync(
    skeyPath,
    JSON.stringify({
      type: SKEY_TYPE,
      description: "Payment Signing Key",
      cborHex,
    }),
  );
  return skeyPath;
};

// ---------------------------------------------------------------------------
// TxOut bridge: app's `TxOut` (with `AddressBech32`) -> konduit `TxOut`
// ---------------------------------------------------------------------------

// Note: the `datum`, `datumhash`, `inlineDatum`, `inlineDatumRaw` and
// `referenceScript` fields from cardano-cli's output are dropped here. The
// konduit `TxOut` is the typed `AlonzoTxOut | BabbageTxOut` variant which has
// its own shape; reconstructing those values from raw strings is a separate
// task outside this slice.
const appTxOutToKonduitTxOut = (txOut: AppTxOut): Result<TxOut, string> =>
  Address.stringCodec
    .deserialise(txOut.address as unknown as string)
    .map((address): TxOut => {
      // We model the app's TxOut as a Babbage-style output without datum and
      // without reference script. The downstream codec only needs the typed
      // shape, and richer datum/scriptRef decoding is a follow-up.
      return {
        address,
        value: txOut.value,
        datumOption: null,
        scriptRef: null,
      } as unknown as TxOut;
    })
    .mapErr((e) => (typeof e === "string" ? e : JSON.stringify(e)));

const appUtxoMapToKonduitUtxos = (
  utxoMap: UtxoMap,
): Result<TransactionUnspentOutput[], string> => {
  const collected: TransactionUnspentOutput[] = [];
  const errors: string[] = [];
  utxoMap.forEach((txOut, txOutRef) => {
    const refStr = txOutRef as unknown as string;
    const [txIdHex, indexStr] = refStr.split("#");
    if (!txIdHex || !indexStr) {
      errors.push(`Invalid TxOutRefHex in UtxoMap: ${refStr}`);
      return;
    }
    const ixNum = parseInt(indexStr, 10);
    if (Number.isNaN(ixNum) || ixNum < 0) {
      errors.push(`Invalid TxIx in TxOutRefHex ${refStr}: ${indexStr}`);
      return;
    }
    const txIxResult = NonNegativeInt.fromNumber(ixNum).mapErr(
      (e) => `Invalid TxIx in TxOutRefHex ${refStr}: ${e}`,
    );
    if (txIxResult.isErr()) {
      errors.push(txIxResult.error);
      return;
    }
    const txIx = TxIx.fromNonNegativeInt(txIxResult.value);
    const txOutResult = appTxOutToKonduitTxOut(txOut);
    if (txOutResult.isErr()) {
      errors.push(`Invalid TxOut for ${refStr}: ${txOutResult.error}`);
      return;
    }
    const inputResult = TxHash.fromHexString(txIdHex as never).map((txHash) =>
      TxInput.fromComponents(txHash, txIx),
    );
    if (inputResult.isErr()) {
      errors.push(`Invalid TxHash in TxOutRefHex ${refStr}: ${JSON.stringify(inputResult.error)}`);
      return;
    }
    collected.push({ input: inputResult.value, output: txOutResult.value });
  });
  if (errors.length > 0) return err(errors.join("; "));
  return ok(collected);
};

// ---------------------------------------------------------------------------
// CardanoCliWallet
// ---------------------------------------------------------------------------

export type CardanoCliWallet = Wallet<CardanoCliWallet.WalletBackend>;
export namespace CardanoCliWallet {
  export type WalletBackend = WalletBackendBase & {
    readonly walletDir: string;
    readonly skeyFile: string;
  };

  const buildBackend = (
    walletDir: string,
    skeyFile: string,
    magic: NetworkMagicNumber,
  ): WalletBackend => ({
    walletDir,
    skeyFile,
    networkMagicNumber: magic,
    getBalance: (vKey: Ed25519VerificationKey) =>
      getBalanceViaCli(vKey, magic),
    submit: (tx: Transaction) => submitViaCli(tx, walletDir, skeyFile),
    utxosAtAddress: (address: Address) =>
      utxosAtAddressViaCli(address, magic),
  });

  const getBalanceViaCli = async (
    vKey: Ed25519VerificationKey,
    magic: NetworkMagicNumber,
  ): Promise<Result<Lovelace, never>> => {
    const network = Network.fromNetworkMagicNumber(magic);
    const pubKeyHash = PubKeyHash.fromPubKey(vKey.key);
    const address: Address = {
      network,
      paymentCredential: { type: "PubKeyHash", hash: pubKeyHash },
    };
    const addressBech32 = AddressBech32.fromAddress(address);
    const utxoMapResult = queryUtxosByAddress(addressBech32, magic);
    if (utxoMapResult.isErr()) {
      // We return zero rather than propagating the cardano-cli error so that
      // polling consumers don't crash on a transient node hiccup. The error
      // is intentionally swallowed here; wallet callers that need the error
      // should query the node directly.
      return ok(0n as Lovelace);
    }
    let total = 0n;
    utxoMapResult.value.forEach((txOut) => {
      total += txOut.value.lovelace as unknown as bigint;
    });
    return ok(total as Lovelace);
  };

  const submitViaCli = async (
    tx: Transaction,
    walletDir: string,
    skeyFile: string,
  ): Promise<Result<TxHash, HttpEndpointError>> => {
    // 1. Extract the tx body bytes (CBOR) and serialise them into a
    //    cardano-cli-compatible JSON envelope inside `walletDir`.
    const cborBytes = tx.toCbor() as unknown as Uint8Array;
    const cborHex = bytesToHex(cborBytes);
    const txBodyFile = nodePath.join(walletDir, "tx.body.json");
    const signedTxFile = nodePath.join(walletDir, "tx.signed.json");
    nodeFs.writeFileSync(
      txBodyFile,
      JSON.stringify({
        type: "Unwitnessed Tx ConwayEra",
        description: "Ledger Cddl Format",
        cborHex,
      }),
    );

    // 2. Sign with cardano-cli using the skey file the backend has on disk.
    const signResult = runCommand(
      `cardano-cli conway transaction sign ` +
        `--tx-body-file "${txBodyFile}" ` +
        `--signing-key-file "${skeyFile}" ` +
        `--out-file "${signedTxFile}"`,
      true,
    );
    if (signResult.isErr()) {
      return err(cliErrorToHttp(signResult.error));
    }

    // 3. Submit the signed envelope and wait for confirmation.
    const submitResult = await submitTxFromEnvelopeFile(signedTxFile);
    if (submitResult.isErr()) {
      return err(cliErrorToHttp(submitResult.error));
    }
    return ok(submitResult.value as unknown as TxHash);
  };

  const utxosAtAddressViaCli = async (
    address: Address,
    magic: NetworkMagicNumber,
  ): Promise<Result<TransactionUnspentOutput[], HttpEndpointError>> => {
    const addressBech32 = AddressBech32.fromAddress(address);
    const result = queryUtxosByAddress(addressBech32, magic);
    if (result.isErr()) {
      return err(cliErrorToHttp(result.error));
    }
    return appUtxoMapToKonduitUtxos(result.value).mapErr(cliErrorToHttp);
  };

  export const createBackend = (
    walletDir: string,
    privateKey: Ed25519PrivateKey,
    networkMagicNumber?: NetworkMagicNumber,
  ): Result<WalletBackend, JsonError> => {
    const socketCheck = requireCardanoSocketPath();
    if (socketCheck.isErr()) return err(socketCheck.error);
    if (!nodeFs.existsSync(walletDir)) {
      nodeFs.mkdirSync(walletDir, { recursive: true });
    }
    const magic = (() => {
      if (networkMagicNumber !== undefined) return ok(networkMagicNumber);
      return resolveNetworkMagicFromEnv();
    })();
    if (magic.isErr()) return err(magic.error);
    const skeyFile = writeSkeyFile(walletDir, privateKey);
    return ok(buildBackend(walletDir, skeyFile, magic.value));
  };

  export function fromPrivateKey(
    walletDir: string,
    privateKey: Ed25519PrivateKey,
    networkMagicNumber?: NetworkMagicNumber,
  ): Result<Wallet<WalletBackend>, JsonError> {
    return createBackend(walletDir, privateKey, networkMagicNumber).map(
      (backend) => new Wallet(privateKey, backend),
    );
  }

  export async function create(
    walletDir: string,
    networkMagicNumber?: NetworkMagicNumber,
  ): Promise<Result<{ wallet: Wallet<WalletBackend>; mnemonic: unknown }, JsonError>> {
    const cardanoKeys = await import("@konduit/cardano-keys");
    const { Ed25519PrivateKey } = await import("@konduit/cardano-keys/rfc8032");
    const mnemonic = cardanoKeys.generateMnemonic("24-words");
    const privateKey = Ed25519PrivateKey.fromMnemonic(mnemonic);
    return fromPrivateKey(walletDir, privateKey, networkMagicNumber).map((wallet) => ({
      wallet,
      mnemonic,
    }));
  }

  export async function restore(
    walletDir: string,
    mnemonic: unknown,
    networkMagicNumber?: NetworkMagicNumber,
  ): Promise<Result<Wallet<WalletBackend>, JsonError>> {
    const { Ed25519PrivateKey } = await import("@konduit/cardano-keys/rfc8032");
    const privateKey = Ed25519PrivateKey.fromMnemonic(mnemonic as never);
    return fromPrivateKey(walletDir, privateKey, networkMagicNumber);
  }
}
