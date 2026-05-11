import { runCommand, runCommandJson } from './exec.js';

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

export const getTip = (): Tip => {
  const tip: Tip = runCommandJson(`cardano-cli query tip`);
  const syncProgress = typeof tip.syncProgress === 'string' ? parseFloat(tip.syncProgress) : tip.syncProgress;
  if (syncProgress < 1) {
    throw new Error(`Node is not fully synchronized (${syncProgress}%)`);
  }
  return tip;
};

export type TxOutRef = string;

export type Utxo = {
  address: string;
  datum: string | null;
  datumhash: string | null;
  inlineDatum: string | null;
  inlineDatumRaw: string | null;
  referenceScript: string | null;
  value: Record<string, Record<string, number>>;
};

export type UtxoMap = {
  [txOutRef: TxOutRef]: Utxo;
};

export const queryUtxoByAddress = (address: string, testnetMagic: number): UtxoMap => {
  return runCommandJson(
    `cardano-cli query utxo --testnet-magic ${testnetMagic} --address "${address}" --output-json`
  );
};

export const queryFirstUtxo = (
  address: string,
  testnetMagic: number
): string => {
  return runCommand(
    `cardano-cli query utxo \
      --testnet-magic "${testnetMagic}" \
      --address "${address}" \
      --out-file /dev/stdout 2>/dev/null \
    | python3 -c "import json,sys; u=json.load(sys.stdin); print(list(u.keys())[0])"`
  ).trim();
};

export const waitForUtxo = async (
  address: string,
  testnetMagic: number,
  delayMs = 1000,
  timeoutMs = 200_000
): Promise<UtxoMap> => {
  const startTime = Date.now();
  while (Date.now() - startTime < timeoutMs) {
    const utxos = queryUtxoByAddress(address, testnetMagic);
    if (Object.keys(utxos).length > 0) {
      return utxos;
    }
    await new Promise((resolve) => setTimeout(resolve, delayMs));
  }
  throw new Error("Timed out waiting for UTxO to appear on chain");
};