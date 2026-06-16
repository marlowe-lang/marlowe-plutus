import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type { Contract } from '@marlowe-lang/language/v1';
import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOk } from '../neverthrow.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Milliseconds } from '@konduit/konduit-consumer/time/duration';
import type { Path } from '../../exec.js';
import { toAsync } from '@konduit/konduit-consumer/neverthrow';
import { waitPatientlyForResultAsync } from '../../neverthrow.js';
import type { Json } from '@konduit/codec/json';
import { ContractState } from '@marlowe-lang/runtime/client';

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
export const init = async (faucetAddr: AddressBech32, faucetSkeyFile: Path) => {
  const timeout = unwrapOk(POSIXMilliseconds.addMilliseconds(POSIXMilliseconds.now(), Milliseconds.fromDigits(6, 0, 0, 0, 0, 0)));
  const contract = mkContract(faucetAddr, timeout);
  const result = await toAsync(marloweRuntimeCli.runInit(contract, faucetAddr, {}, null, true))
    .andThen((response) => cardanoCli.signTxEnvelope(faucetSkeyFile, response.tx).map((signedTxEnvelope) => {
        return {
          contractId: response.contractId,
          txEnvelope: signedTxEnvelope,
        };
      }))
    .andThen(({ txEnvelope, contractId }) => cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId))
    .andThen((contractId) => waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractId, {}, null, true)),
        (_res: ContractState) => true,
        { timeoutMs: 120_000, everyMs: 5_000 }
      ),
    );
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

