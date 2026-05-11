import { execCli, CliArgs, Path } from './exec.js';

function getMarloweCli(repoRoot: string): string {
  if (process.env.MARLOWE_CLI) {
    return process.env.MARLOWE_CLI;
  }
  return `cabal run -v0 --project-file ${repoRoot}/cabal.project marlowe-cli --`;
}

function execMarloweCli(repoRoot: string, args: CliArgs): string {
  const marloweCli = getMarloweCli(repoRoot);
  return execCli(marloweCli, args);
}

export function getMarloweScriptAddress(repoRoot: string): string {
  return execMarloweCli(repoRoot, [
    '--conway-era', 'contract', 'address',
    ['--testnet-magic', '$CARDANO_TESTNET_MAGIC']
  ]).trim();
}

export function getTxIdFromTxBodyFile(repoRoot: string, txBodyFile: Path): string {
  const result = execMarloweCli(repoRoot, [
    'transaction', 'txid',
    '--tx-body-file', txBodyFile
  ]);
  return result.trim();
}

export function runInitialize(
  repoRoot: string,
  contractFile: Path,
  stateFile: Path,
  outFile: Path,
  options: {
    testnetMagic: number;
    socketPath: string;
  }
): void {
  execMarloweCli(repoRoot, [
    '--conway-era', 'run', 'initialize',
    ['--testnet-magic', options.testnetMagic],
    ['--socket-path', options.socketPath],
    ['--contract-file', contractFile],
    ['--state-file', stateFile],
    ['--out-file', outFile],
    '--print-stats'
  ]);
}

export function runExecute(
  repoRoot: string,
  marloweOutFile: Path,
  txIn: string,
  requiredSigner: Path,
  changeAddress: string,
  outFile: Path,
  options: {
    testnetMagic: number;
    socketPath: string;
    txInMarlowe?: string;
    txInCollateral?: string;
    marloweInFile?: Path;
    submit?: number;
  }
): void {
  const args: CliArgs = [
    '--conway-era', 'run', 'execute',
    ['--testnet-magic', options.testnetMagic],
    ['--socket-path', options.socketPath],
    ['--tx-in', txIn],
    ['--required-signer', requiredSigner],
    ['--change-address', changeAddress],
    ['--out-file', outFile],
    ['--marlowe-out-file', marloweOutFile],
    '--print-stats'
  ];
  if (options.marloweInFile) {
    args.push(['--marlowe-in-file', options.marloweInFile]);
  }
  if (options.txInMarlowe) {
    args.push(['--tx-in-marlowe', options.txInMarlowe]);
  }
  if (options.txInCollateral) {
    args.push(['--tx-in-collateral', options.txInCollateral]);
  }
  if (options.submit !== undefined) {
    args.push(['--submit', options.submit]);
  }
  execMarloweCli(repoRoot, args);
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
): void {
  execMarloweCli(repoRoot, [
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