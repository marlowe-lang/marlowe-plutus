import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type { Contract } from '@marlowe-lang/language/v1';
import * as lang from '@marlowe-lang/language/v1';
import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOk } from '../neverthrow.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Milliseconds } from '@konduit/konduit-consumer/time/duration';
import type { Path } from '../../exec.js';
import { toAsync } from '@konduit/konduit-consumer/neverthrow';
import { waitPatientlyForResultAsync } from '../../neverthrow.js';
import { ApplyInputsResponse, ContractId, ContractState } from '@marlowe-lang/runtime/client';

function mkContract(partyAddr: AddressBech32, timeout: POSIXMilliseconds): Contract {
  return {
    timeout: BigInt(timeout),
    timeout_continuation: "close",
    when: [
      {
        case: {
          deposits: 12000000n,
          into_account: { address: partyAddr },
          of_token: { currency_symbol: "", token_name: "" },
          party: { address: partyAddr }
        },
        then: "close"
      }
    ]
  };
}

// Let's port the above to a much simpler flow which uses our marloweRuntimeCli client.
export const run = async (faucetAddr: AddressBech32, faucetSkeyFile: Path) => {
  const timeout = unwrapOk(POSIXMilliseconds.addMilliseconds(POSIXMilliseconds.now(), Milliseconds.fromDigits(6, 0, 0, 0, 0)));
  const contract = mkContract(faucetAddr, timeout);
  const result = await
    toAsync(marloweRuntimeCli.runInit(contract, faucetAddr, {}, null, true))
    .andThen((response) => cardanoCli.signTxEnvelope(faucetSkeyFile, response.tx).map((signedTxEnvelope) => {
        return {
          contractId: response.contractId,
          txEnvelope: signedTxEnvelope,
        };
      }))
    .andThen(({ txEnvelope, contractId }) => cardanoCli.submitTxEnvelope(txEnvelope, true).map(() => contractId))
    .andThen((contractId) => waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractId, {}, null, true)),
        (_res: ContractState) => true,
        { timeoutMs: 120_000, everyMs: 5_000 }
      ),
    ).andThen((contractState: ContractState) => {
      console.log("initial conract state:");
      console.log(contractState);
      // // from lang:
      //  export type AccountId = Party;
      //  export interface Token {
      //    currency_symbol: PolicyId;
      //    token_name: TokenName;
      //  }
      //  export type Party = Address | Role;
      //  export interface IDeposit {
      //    input_from_party: Party;
      //    that_deposits: bigint;
      //    of_token: Token;
      //    into_account: AccountId;
      //  }
      const input = {
        input_from_party: { address: faucetAddr },
        that_deposits: 12000000n,
        of_token: lang.adaToken,
        into_account: { address: faucetAddr }
      };

      // // from runtime:
      // export type ApplyInputsResponse = {
      //   contractId: ContractId;
      //   transactionId: string; // FIXME: Use cardano.TxId.jsonCodec when available
      //   tx: TxEnvelope;
      //   safetyErrors: Json[]; // FIXME: Use SafetyError type if available
      // };
      //
      // // from marloweRuntimeCli:
      // export function runApplyInputs(
      //   marloweInputs: NormalInput[],
      //   contractId: ContractId,
      //   userWalletAddress: AddressBech32,
      //   options: {
      //     outputDir?: string;
      //     serverHost?: string;
      //     serverPort?: PositiveInt;
      //     socketPath?: string;
      //     testnetMagic?: TestnetMagic;
      //   },
      //   repoRoot: Path | null = null,
      //   debug: boolean = false
      // ): Result<ApplyInputsResponse, JsonError | CommandError | string> {
      //
      //
      return marloweRuntimeCli.runApplyInputs([input], contractState.contractId, faucetAddr, {}, null, true);
    }).andThen((applyInputsResponse: ApplyInputsResponse) => {
      //sign and submit
      return toAsync(cardanoCli.signTxEnvelope(faucetSkeyFile, applyInputsResponse.tx))
        .andThen((signedTxEnvelope) => cardanoCli.submitTxEnvelope(signedTxEnvelope))
        .map(() => applyInputsResponse.contractId);
    }).andThen((contractId: ContractId) => {
      // Wait for the contract state to update after applying inputs
      return waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractId, {}, null, true)),
        (updatedContractState: ContractState) => (
            updatedContractState.state === null
             && updatedContractState.currentContract === null
        ),
        { timeoutMs: 120_000, everyMs: 5_000 }
      );
    });

  result.match(
    (contractState: ContractState) => {
      console.log(contractState);
    },
    (error) => {
      if (typeof error === 'object' && error !== null && 'stderr' in error) {
        console.error(error.stderr);
      } else {
        console.error(error);
      }
      throw new Error(`Failed to initialize contract and submit transaction: ${error}`);
    }
  );
}

