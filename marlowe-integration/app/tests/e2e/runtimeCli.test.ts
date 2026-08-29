import { test, beforeAll, afterAll } from 'vitest'
import * as deposit from '../../src/testing/e2e/deposit.js';
import * as bet from '../../src/testing/e2e/bet.js';
import * as init from '../../src/testing/e2e/init.js';
import * as fs from 'node:fs'
import { AddressBech32, NetworkMagicNumber } from '@konduit/konduit-consumer/cardano';
import { unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import { PositiveBigInt } from '@konduit/codec/integers/big';
import path from 'path';
import { Path } from '../../src/exec.js';
import * as cardanoCli from '../../src/cardanoCli.js';
import type { Wallet } from '../../src/cardano.js';

const parseEnv = (): TestEnv => {
  const repoRoot = process.env.ROOT_DIR || process.cwd();
  const networkMagicNumber = unwrapOrPanicWith(
    PositiveBigInt.fromString(process.env.CARDANO_NODE_NETWORK_ID || '42').map(NetworkMagicNumber.fromPositiveBigInt),
    (err) => `Failed to parse CARDANO_NODE_NETWORK_ID: ${process.env.CARDANO_NODE_NETWORK_ID}, ${err}`
  );
  const slotLength = parseInt(process.env.SLOT_LENGTH_MS || '200', 10)
  const nodeSocketPath = (() => {
    const nodeSocketPath = process.env.CARDANO_NODE_SOCKET_PATH;
    if(!nodeSocketPath) throw new Error("CARDANO_NODE_SOCKET_PATH environment variable is not set");
    return nodeSocketPath;
  })();
  const preserveTempDir = process.env.PRESERVE_TEMP_DIR === 'true';
  const faucetAddr = (() => {
    const faucetAddrFileStr = process.env.FAUCET_ADDR_FILE;
    if(!faucetAddrFileStr) throw new Error("FAUCET_ADDR_FILE environment variable is not set");
    if(!fs.existsSync(faucetAddrFileStr)) throw new Error(`FAUCET_ADDR_FILE does not exist: ${faucetAddrFileStr}`);
    const faucetAddrStr = fs.readFileSync(faucetAddrFileStr, 'utf-8').trim();
    return unwrapOrPanicWith(AddressBech32.fromString(faucetAddrStr), (err) => `Failed to parse FAUCET_ADDR: ${faucetAddrStr}, ${err}`);
  })();
  const faucetSkeyFile = (() => {
    let faucetSkeyFile = process.env.FAUCET_SKEY_FILE;
    if(!faucetSkeyFile) throw new Error("FAUCET_SKEY_FILE environment variable is not set");
    if(!fs.existsSync(faucetSkeyFile)) throw new Error(`FAUCET_SKEY_FILE does not exist: ${faucetSkeyFile}`);
    return faucetSkeyFile as Path;
  })();
  return {
    faucetAddr,
    faucetSkeyFile,
    networkMagicNumber,
    nodeSocketPath,
    preserveTempDir,
    repoRoot,
    slotLength,
  };
}

type TestEnv = {
  faucetAddr: AddressBech32;
  faucetSkeyFile: Path;
  networkMagicNumber: NetworkMagicNumber;
  nodeSocketPath: string;
  preserveTempDir: boolean;
  repoRoot: string;
  slotLength: number;
}

type TestContext = {
  env: TestEnv;
  tempDir: string;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
}

let ctx: TestContext;

const PARTY_FUNDING_AMOUNT_LOVELACE = 10_000_000; // 10 ADA per wallet

beforeAll(async () => {
  const testEnv = parseEnv();
  const tempDir = path.join(testEnv.repoRoot, 'marlowe-integration', 'test-temp')
  if(!fs.existsSync(tempDir)) fs.mkdirSync(tempDir, { recursive: true });

  const party1 = cardanoCli.createWallet(testEnv.networkMagicNumber, tempDir);
  const party2 = cardanoCli.createWallet(testEnv.networkMagicNumber, tempDir);
  const oracle = cardanoCli.createWallet(testEnv.networkMagicNumber, tempDir);

  const fundingResult = await cardanoCli.transferFunds(
    testEnv.faucetAddr,
    testEnv.faucetSkeyFile,
    [
      { amount: PARTY_FUNDING_AMOUNT_LOVELACE, address: party1.addr },
      { amount: PARTY_FUNDING_AMOUNT_LOVELACE, address: party2.addr },
      { amount: PARTY_FUNDING_AMOUNT_LOVELACE, address: oracle.addr },
    ],
    undefined,
    testEnv.networkMagicNumber,
  );
  fundingResult.match(
    (_txId) => {},
    (err) => { throw new Error(`Funding wallets from faucet failed: ${String(err)}`); },
  );

  ctx = { env: testEnv, tempDir, party1, party2, oracle }
});

afterAll(async () => {
  if (ctx && !ctx.env.preserveTempDir && ctx?.tempDir && fs.existsSync(ctx.tempDir)) {
    fs.rmSync(ctx.tempDir, { recursive: true })
  }
})

// test('Init lifecycle using marloweRuntimeCli', { tags: ['lifecycle', 'marlowe-runtime-cli'], timeout: 120000, }, async () => {
//   await init.run(ctx.env.faucetAddr, ctx.env.faucetSkeyFile);
// })

// test('Deposit lifecycle using marloweRuntimeCli', { tags: ['lifecycle', 'marlowe-runtime-cli'], timeout: 120000, }, async () => {
//   await deposit.run(ctx.env.faucetAddr, ctx.env.faucetSkeyFile);
// })

test('Bet lifecycle using marloweRuntimeCli', { tags: ['lifecycle', 'marlowe-runtime-cli'], timeout: 120000, }, async () => {
  const faucet: Wallet = { addr: ctx.env.faucetAddr, skeyFile: ctx.env.faucetSkeyFile };
  await bet.run({
    amount: 5_000_000n,
    party1: ctx.party1,
    party2: ctx.party2,
    oracle: ctx.oracle,
    faucet,
    winningChoice: 'no-winners',
  });
})
