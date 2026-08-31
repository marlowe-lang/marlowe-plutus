import type { Result } from 'neverthrow';
import { execCli, type CliArgs, type CommandError, type Path } from './exec.js';
import { TxOutRefHex, type NetworkMagicNumber, type TxOutRef } from '@konduit/konduit-consumer/cardano';

function getMarloweCli(repoRoot: string): string {
  if (process.env.MARLOWE_CLI) {
    return process.env.MARLOWE_CLI;
  }
  return `cabal run -v0 --project-file ${repoRoot}/cabal.project marlowe-cli --`;
}

function execMarloweCli(repoRoot: string, args: CliArgs, debug: boolean = false): Result<string, CommandError> {
  const marloweCli = getMarloweCli(repoRoot);
  return execCli(marloweCli, args, debug);
}

export function getMarloweScriptAddress(repoRoot: string, testnetMagic: number): Result<string, CommandError> {
  return execMarloweCli(repoRoot, [
    '--conway-era', 'contract', 'address',
    ['--testnet-magic', testnetMagic]
  ]);
}

export function runInitialize(
  repoRoot: string,
  contractFile: Path,
  stateFile: Path,
  outFile: Path,
  options: {
    testnetMagic: NetworkMagicNumber;
    socketPath: string;
  },
  debug: boolean = false
): Result<string, CommandError> {
  return execMarloweCli(repoRoot, [
    '--conway-era', 'run', 'initialize',
    ['--testnet-magic', options.testnetMagic],
    ['--socket-path', options.socketPath],
    ['--contract-file', contractFile],
    ['--state-file', stateFile],
    ['--out-file', outFile],
    '--print-stats'
  ], debug);
}

const flattenTxOutRef = (txOutRef: TxOutRefHex | TxOutRef): TxOutRefHex => {
  if (typeof txOutRef === 'string') {
    return txOutRef;
  } else {
    return TxOutRefHex.fromTxOutRef(txOutRef);
  }
}

export function runExecute(
  repoRoot: string,
  marloweOutFile: Path,
  txIn: TxOutRefHex | TxOutRef,
  requiredSigner: Path,
  changeAddress: string,
  outFile: Path,
  options: {
    testnetMagic: number;
    socketPath: string;
    txInMarlowe?: TxOutRefHex | TxOutRef;
    txInCollateral?: TxOutRefHex | TxOutRef;
    marloweInFile?: Path;
    submit?: number;
  },
  debug: boolean = false
): Result<string, CommandError> {
  const args: CliArgs = [
    '--conway-era', 'run', 'execute',
    ['--testnet-magic', options.testnetMagic],
    ['--socket-path', options.socketPath],
    ['--tx-in', flattenTxOutRef(txIn)],
    ['--required-signer', requiredSigner],
    ['--change-address', changeAddress],
    ['--out-tx-file', outFile],
    ['--marlowe-out-file', marloweOutFile],
    '--print-stats'
  ];
  if (options.marloweInFile) {
    args.push(['--marlowe-in-file', options.marloweInFile]);
  }
  if (options.txInMarlowe) {
      args.push(['--tx-in-marlowe', flattenTxOutRef(options.txInMarlowe)]);
  }
  if (options.txInCollateral) {
    args.push(['--tx-in-collateral', flattenTxOutRef(options.txInCollateral)]);
  }
  if (options.submit !== undefined) {
    args.push(['--submit', options.submit]);
  }
  return execMarloweCli(repoRoot, args, debug);
}

export function runPrepare(
  repoRoot: string,
  marloweFile: Path,
  depositAccount: string,
  depositParty: string,
  depositAmount: number,
  outFile: Path,
  options: {
    invalidBefore: number;
    invalidHereafter: number;
  }
): Result<string, CommandError> {
  return execMarloweCli(repoRoot, [
    '--conway-era', 'run', 'prepare',
    ['--marlowe-file', marloweFile],
    ['--deposit-account', depositAccount],
    ['--deposit-party', depositParty],
    ['--deposit-amount', depositAmount],
    ['--out-file', outFile],
    ['--invalid-before', options.invalidBefore],
    ['--invalid-hereafter', options.invalidHereafter],
    '--print-stats'
  ]);
}

