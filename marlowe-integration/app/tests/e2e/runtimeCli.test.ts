import { test, beforeAll, afterAll } from 'vitest'
import * as deposit from '../../src/testing/e2e/deposit.js';
import * as fs from 'node:fs'
import { AddressBech32, NetworkMagicNumber } from '@konduit/konduit-consumer/cardano';
import { unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import { PositiveBigInt } from '@konduit/codec/integers/big';
import path from 'path';

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
    const faucetSkeyFile = process.env.FAUCET_SKEY_FILE;
    if(!faucetSkeyFile) throw new Error("FAUCET_SKEY_FILE environment variable is not set");
    if(!fs.existsSync(faucetSkeyFile)) throw new Error(`FAUCET_SKEY_FILE does not exist: ${faucetSkeyFile}`);
    return faucetSkeyFile;
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
  faucetSkeyFile: string;
  networkMagicNumber: NetworkMagicNumber;
  nodeSocketPath: string;
  preserveTempDir: boolean;
  repoRoot: string;
  slotLength: number;
}

type TestContext = {
  env: TestEnv;
  tempDir: string;
}

let ctx: TestContext;

beforeAll(async () => {
  const testEnv = parseEnv();
  const tempDir = path.join(testEnv.repoRoot, 'marlowe-integration', 'test-temp')
  if(!fs.existsSync(tempDir)) fs.mkdirSync(tempDir, { recursive: true });
  ctx = { env: testEnv, tempDir }
});

afterAll(async () => {
  if (ctx && !ctx.env.preserveTempDir && ctx?.tempDir && fs.existsSync(ctx.tempDir)) {
    fs.rmSync(ctx.tempDir, { recursive: true })
  }
})

test('Deposit lifecycle using marloweRuntimeCli', { tags: ['lifecycle', 'marlowe-runtime-cli'], timeout: 120000, }, async () => {
  await deposit.init(ctx.env.faucetAddr, ctx.env.faucetSkeyFile);
})
