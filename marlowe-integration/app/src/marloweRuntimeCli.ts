import type { Tagged } from 'type-fest';
import { type AddressBech32, type TestnetMagic } from './cardano.js';
import { execCli, type CliArgs, type CommandError, type Path } from './exec.js';
import * as fs from 'node:fs'
import type { PositiveInt } from '@konduit/codec/integers/smallish';
import * as json from '@konduit/codec/json';
import { err, ok, type Result } from 'neverthrow';
import { Json } from '@konduit/codec/json';
import type { Contract } from '@marlowe-lang/language/v1';
import { ContractId, ContractState, PostCreateContractResponse, ApplyInputsResponse } from '@marlowe-lang/runtime/client';
import type { JsonError } from '@konduit/codec/json/codecs';
import type { NormalInput } from '@marlowe-lang/language/v1';
import { tmpdir } from 'node:os';
import * as path from 'node:path';

function getMarloweRuntimeClient(repoRoot: Path | null = null): string {
  if (process.env.MARLOWE_RUNTIME_CLIENT) {
    return process.env.MARLOWE_RUNTIME_CLIENT;
  }
  repoRoot = repoRoot ?? process.env.ROOT_DIR ?? process.cwd();
  return `cabal run -v0 --project-file ${repoRoot}/cabal.project marlowe-runtime:cli --`;
}

export function execMarloweRuntimeClient(args: CliArgs, repoRoot: Path | null = null, debug: boolean = false): Result<string, CommandError> {
  const marloweCli = getMarloweRuntimeClient(repoRoot);
  return execCli(marloweCli, args, debug);
}

export function writeContractFile(contractFile: Path, contract: Contract): MarloweContractFile {
  fs.writeFileSync(contractFile, json.stringify(contract as Json, undefined, 2))
  return contractFile as MarloweContractFile;
}

export type MarloweContractFile = Tagged<Path, 'MarloweContractFile'>;

const mkTempDir = (local: boolean = false): string => {
  const rootDir = local ? process.cwd() : tmpdir();
  const prefix = path.join(rootDir, 'marlowe-runtime-client-temp-');
  return fs.mkdtempSync(prefix);
}

// $ cabal run marlowe-runtime-client -- contract init --help
// Usage: marlowe-runtime-client contract init
//          --contract-file CONTRACT_FILE [--devel-scripts]
//          --funding-wallet-address BECH32 [--message-format text|json|yaml]
//          [--mainnet | --testnet-magic INTEGER] [-s|--socket-path ARG]
//          [-o|--output-dir ARG]
// 
//   Build initial marlowe transaction
// 
// Available options:
//   --contract-file CONTRACT_FILE
//                            JSON input file for the contract.
//   --devel-scripts          Compile the devel script variants with tracing
//                            preserved.
//   --funding-wallet-address BECH32
//                            The address which pays for claim transactions.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   --mainnet                Execute on mainnet.
//   --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
//                            environment variable's value.
//   -s,--socket-path ARG     Location of the cardano-node socket file. Defaults to
//                            the CARDANO_NODE_SOCKET_PATH environment variable's
//                            value.
//                            (default: "/home/paluh/projects/marlowe/marlowe-plutus/.run/testnet/state-cluster9/bft1.socket")
//   -o,--output-dir ARG      Directory where transaction file will be written.
//                            (default: "out")
//   -h,--help                Show this help text
export function runInitCLI(
  contractFile: MarloweContractFile,
  fundingWalletAddress: AddressBech32,
  options: {
    develScripts?: boolean;
    outputDir?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: TestnetMagic;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'contract', 'init',
    ['--contract-file', contractFile],
    ['--message-format', 'json'],
    ['--funding-wallet-address', fundingWalletAddress],
  ];
  if (options.develScripts) {
    args.push('--devel-scripts');
  }
  if (options.testnetMagic) {
    args.push(['--testnet-magic', options.testnetMagic]);
  }
  if (options.socketPath) {
    args.push(['--socket-path', options.socketPath]);
  }
  if (options.outputDir) {
    args.push(['--output-dir', options.outputDir]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

export function runInit(
  contract: Contract,
  fundingWalletAddress: AddressBech32,
  options: {
    develScripts?: boolean;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: TestnetMagic;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostCreateContractResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const contractFile = writeContractFile(`${tmpDir}/contract.json`, contract);
  const finalOptions = {
    outputDir: tmpDir,
    ...options,
  }
  return runInitCLI(contractFile, fundingWalletAddress, finalOptions, repoRoot, debug)
    .andThen(json => PostCreateContractResponse.jsonCodec.deserialise(json));
}
// Usage: cli contract get --contract-id CONTRACT_ID
//                         [--message-format text|json|yaml]
//                         [-h|--server-host HOST] [-p|--server-port PORT]
// 
//   Get the current state of a Marlowe contract from the web server.
// 
// Available options:
//   --contract-id CONTRACT_ID
//                            The id of the Marlowe contract to query, in the
//                            format <txId>#<txIx>.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -h,--help                Show this help text
//
export const runGetCLI = (
  contractId: ContractId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> => {
  const args: CliArgs = [
    'contract', 'get',
    ['--contract-id', contractId],
    ['--message-format', 'json'],
  ];
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => {
    return Json.fromString(jsonStr).match(
      (json) => ok(json),
      (e) => {
        console.error("Failed to parse JSON output from marlowe-runtime-client:");
        console.error(e);
        console.error("Original output:");
        console.error(jsonStr);
        return err(e);
      }
    );
  });
}

export function runGet(
  contractId: ContractId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractState, JsonError | CommandError | string> {
  return runGetCLI(contractId, options, repoRoot, debug).andThen(json => ContractState.jsonCodec.deserialise(json));
}

// Usage: cli contract apply-inputs (-i|--marlowe-inputs-file MARLOWE_INPUTS_FILE)
//                                  --contract-id CONTRACT_ID
//                                  [--message-format text|json|yaml]
//                                  [--mainnet | --testnet-magic INTEGER]
//                                  [-s|--socket-path ARG] [-o|--output-dir ARG]
//                                  [-h|--server-host HOST] [-p|--server-port PORT]
//                                  (-w|--user-wallet-address BECH32)
// 
//   Build input application transaction
// 
// Available options:
//   -i,--marlowe-inputs-file MARLOWE_INPUTS_FILE
//                            JSON file which contains a list of Marlowe contract
//                            inputs which should be applied.
//   --contract-id CONTRACT_ID
//                            The id of the Marlowe contract to query, in the
//                            format <txId>#<txIx>.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   --mainnet                Execute on mainnet.
//   --testnet-magic INTEGER  Network magic. Defaults to the
//                            CARDANO_NODE_NETWORK_ID environment variable's value.
//   -s,--socket-path ARG     Location of the cardano-node socket file. Defaults to
//                            the CARDANO_NODE_SOCKET_PATH environment variable's
//                            value.
//                            (default: "/home/paluh/projects/marlowe/marlowe-plutus/.run/testnet/state-cluster9/bft1.socket")
//   -o,--output-dir ARG      Directory where transaction file will be written.
//                            (default: "out")
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -w,--user-wallet-address BECH32
//                            The address which both pays for the transaction and
//                            executes the contract action.
//   -h,--help                Show this help text
export function runApplyInputsCLI(
  marloweInputsFile: Path,
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  options: {
    outputDir?: string;
    serverHost?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: TestnetMagic;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'contract', 'apply-inputs',
    ['--marlowe-inputs-file', marloweInputsFile],
    ['--contract-id', contractId],
    ['--user-wallet-address', userWalletAddress],
    ['--message-format', 'json'],
  ];
  if (options.outputDir) {
    args.push(['--output-dir', options.outputDir]);
  }
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  if (options.socketPath) {
    args.push(['--socket-path', options.socketPath]);
  }
  if (options.testnetMagic) {
    args.push(['--testnet-magic', options.testnetMagic]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

export function runApplyInputs(
  marloweInputs: NormalInput[],
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: TestnetMagic;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ApplyInputsResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const marloweInputsFile = `${tmpDir}/marlowe-inputs.json`;
  fs.writeFileSync(marloweInputsFile, json.stringify(marloweInputs as Json, undefined, 2));
  const finalOptions = {
    outputDir: tmpDir,
    ...options,
  }
  return runApplyInputsCLI(marloweInputsFile, contractId, userWalletAddress, finalOptions, repoRoot, debug)
    .andThen(json => {
      console.debug(json);
      return ApplyInputsResponse.jsonCodec.deserialise(json);
    });
}
