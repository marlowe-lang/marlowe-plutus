import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type {
  Contract,
  IChoice,
  IDeposit,
  NormalInput,
  Observation,
  Value,
} from '@marlowe-lang/language/v1';
import * as lang from '@marlowe-lang/language/v1';
import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import * as cardanoCli from '../../cardanoCli.js';
import { unwrapOk } from '../neverthrow.js';
import { POSIXMilliseconds } from '@konduit/konduit-consumer/time/absolute';
import { Milliseconds } from '@konduit/konduit-consumer/time/duration';
import type { Path } from '../../exec.js';
import { toAsync } from '@konduit/konduit-consumer/neverthrow';
import type { ResultAsync } from 'neverthrow';
import { waitPatientlyForResultAsync } from '../../neverthrow.js';
import {
  ApplyInputsResponse,
  ContractId,
  ContractState,
  PostCreateContractResponse,
} from '@marlowe-lang/runtime/client';

function mkDepositContract(opts: {
  partyAddr: AddressBech32;
  amount: bigint;
  timeout: POSIXMilliseconds;
  continuation: Contract;
}): Contract {
  const { partyAddr, amount, timeout, continuation } = opts;
  return {
    when: [
      {
        case: {
          party: { address: partyAddr },
          deposits: amount,
          of_token: lang.lovelace,
          into_account: { address: partyAddr },
        },
        then: continuation,
      },
    ],
    timeout: BigInt(timeout),
    timeout_continuation: "close",
  };
}

function mkChoiceContract(opts: {
  partyAddr: AddressBech32;
  choiceName: string;
  bounds: { from: bigint; to: bigint }[];
  timeout: POSIXMilliseconds;
  continuation: Contract;
}): Contract {
  const { partyAddr, choiceName, bounds, timeout, continuation } = opts;
  return {
    when: [
      {
        case: {
          choose_between: bounds,
          for_choice: {
            choice_name: choiceName,
            choice_owner: { address: partyAddr },
          },
        },
        then: continuation,
      },
    ],
    timeout: BigInt(timeout),
    timeout_continuation: "close",
  };
}

// Builder: the settlement contract. Branches on the oracle's choice value:
//   1 -> pay `amount` from account(party2Addr) to party(party1Addr), then
//        close (party1 also recovers their own deposit via close's refund).
//   2 -> symmetric.
//   otherwise (0 / no winners, or the oracle timed out, though timeout would
//   have already reduced to close) -> close, which refunds both accounts.
function mkSettlementContract(opts: {
  party1Addr: AddressBech32;
  party2Addr: AddressBech32;
  amount: bigint;
  choiceName: string;
  party1WinsChoice: bigint;
  party2WinsChoice: bigint;
  oracleAddr: AddressBech32;
}): Contract {
  const { party1Addr, party2Addr, amount, choiceName, party1WinsChoice, party2WinsChoice, oracleAddr } = opts;

  const choiceValue: Value = {
    value_of_choice: {
      choice_name: choiceName,
      choice_owner: { address: oracleAddr },
    },
  };
  const team1Wins: Observation = { value: choiceValue, equal_to: party1WinsChoice };
  const team2Wins: Observation = { value: choiceValue, equal_to: party2WinsChoice };

  const payLoserStakeToWinner = (winner: AddressBech32, loser: AddressBech32): Contract => ({
    pay: amount,
    token: lang.lovelace,
    from_account: { address: loser },
    to: { party: { address: winner } },
    then: "close",
  });

  return {
    if: team1Wins,
    then: payLoserStakeToWinner(party1Addr, party2Addr),
    else: {
      if: team2Wins,
      then: payLoserStakeToWinner(party2Addr, party1Addr),
      else: "close",
    },
  };
}

// Oracle choice encoding for the bet:
//   0 = no winners (close refunds each party from their own account)
//   1 = team 1 won (party2's stake is paid to party1)
//   2 = team 2 won (party1's stake is paid to party2)
const CHOICE_NAME = "team-1-vs-team-2";
const NO_WINNERS = 0n;
const TEAM_1_WINS = 1n;
const TEAM_2_WINS = 2n;

// Builds a two-party betting contract settled by an oracle.
//
// Each party deposits `amount` of lovelace into their own account (sequentially,
// party1 first, then party2). Once both deposits are in, the oracle submits
// a single numeric choice in [0,2] via a Choice action:
//   0 -> close (both parties refunded from their own accounts)
//   1 -> party2's stake is paid to party1, then close (party1's own deposit
//        is also refunded from their account by close)
//   2 -> symmetric
//
// `timeout` is the absolute POSIX-ms deadline applied to every stage. Each
// stage's `timeout_continuation` is `close`, so any participant failing to act
// in time simply causes the contract to close and refund whatever has already
// been deposited.
function mkContract(
  amount: bigint,
  party1Addr: AddressBech32,
  party2Addr: AddressBech32,
  oracleAddr: AddressBech32,
  timeout: POSIXMilliseconds
): Contract {
  return mkDepositContract({
    partyAddr: party1Addr,
    amount,
    timeout,
    continuation: mkDepositContract({
      partyAddr: party2Addr,
      amount,
      timeout,
      continuation: mkChoiceContract({
        partyAddr: oracleAddr,
        choiceName: CHOICE_NAME,
        bounds: [{ from: NO_WINNERS, to: TEAM_2_WINS }],
        timeout,
        continuation: mkSettlementContract({
          party1Addr,
          party2Addr,
          amount,
          choiceName: CHOICE_NAME,
          party1WinsChoice: TEAM_1_WINS,
          party2WinsChoice: TEAM_2_WINS,
          oracleAddr,
        })
      })
    })
  });
}

type Wallet = {
  addr: AddressBech32;
  skeyFile: Path;
};

type WinningChoice = "no-winners" | "party1-wins" | "party2-wins";

namespace WinningChoice {
  export const toChoiceValue = (choice: WinningChoice): bigint => {
    switch (choice) {
      case "no-winners":
        return NO_WINNERS;
      case "party1-wins":
        return TEAM_1_WINS;
      case "party2-wins":
        return TEAM_2_WINS;
    }
  }
}

type RunOpts = {
  amount: bigint;
  party1: Wallet;
  party2: Wallet;
  oracle: Wallet;
  faucet: Wallet;
  // The numeric value the oracle will submit: 0n (no winners), 1n
  // (party1 wins), or 2n (party2 wins).
  winningChoice: WinningChoice;
};

const POLL_TIMEOUT_MS = 120_000;
const POLL_EVERY_MS = 5_000;
const CONTRACT_TIMEOUT = Milliseconds.fromDigits(6, 0, 0, 0, 0, 0);

const mkContractTimeout = (): POSIXMilliseconds =>
  unwrapOk(
    POSIXMilliseconds.addMilliseconds(POSIXMilliseconds.now(), CONTRACT_TIMEOUT),
  );

// Builds the IDeposit input a party uses to fund their own account.
const mkDepositInput = (partyAddr: AddressBech32, amount: bigint): IDeposit => ({
  input_from_party: { address: partyAddr },
  that_deposits: amount,
  of_token: lang.lovelace,
  into_account: { address: partyAddr },
});

// Builds the IChoice input the oracle uses to settle the bet.
const mkChoiceInput = (oracleAddr: AddressBech32, chosenNum: bigint): IChoice => ({
  for_choice_id: {
    choice_name: CHOICE_NAME,
    choice_owner: { address: oracleAddr },
  },
  input_that_chooses_num: chosenNum,
});

// Initializes the bet contract via the marlowe-runtime CLI, signs the init
// transaction with the faucet wallet, submits it, and waits until the contract
// appears in the runtime (`runGet` returns Ok).
const initBetContract = (opts: {
  amount: bigint;
  party1Addr: AddressBech32;
  party2Addr: AddressBech32;
  oracleAddr: AddressBech32;
  faucet: Wallet;
}): ResultAsync<ContractState, unknown> => {
  const { amount, party1Addr, party2Addr, oracleAddr, faucet } = opts;
  const contract = mkContract(
    amount,
    party1Addr,
    party2Addr,
    oracleAddr,
    mkContractTimeout(),
  );
  return toAsync(marloweRuntimeCli.runInit(contract, faucet.addr, {}, null, true))
    .andThen((response: PostCreateContractResponse) =>
      cardanoCli.signTxEnvelope(faucet.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ contractId, txEnvelope }) =>
      cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId),
    )
    .andThen(contractId =>
      waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractId, {}, null, true)),
        (_state: ContractState) => true,
        { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
      ),
    );
};

// Applies a single deposit input on behalf of `party`. Signs the apply-inputs
// transaction with the party's skey, submits it, and waits until the runtime
// reflects the new state.
const makeDeposit = (opts: {
  contractId: ContractId;
  party: Wallet;
  amount: bigint;
}): ResultAsync<ContractState, unknown> => {
  const { contractId, party, amount } = opts;
  const input: NormalInput = mkDepositInput(party.addr, amount);
  return toAsync(
    marloweRuntimeCli.runApplyInputs([input], contractId, party.addr, {}, null, true),
  )
    .andThen((response: ApplyInputsResponse) =>
      cardanoCli.signTxEnvelope(party.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ txEnvelope }) => cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId))
    .andThen(contractIdAfter =>
      waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractIdAfter, {}, null, true)),
        (state: ContractState) => state.contractId === contractId,
        { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
      ),
    );
};

// Submits the oracle's choice via apply-inputs, signs it with the oracle's
// skey, and waits for the final contract state.
const applyChoice = (opts: {
  contractId: ContractId;
  oracle: Wallet;
  choiceValue: bigint;
}): ResultAsync<ContractState, unknown> => {
  const { contractId, oracle, choiceValue } = opts;
  const input: NormalInput = mkChoiceInput(oracle.addr, choiceValue);
  return toAsync(
    marloweRuntimeCli.runApplyInputs([input], contractId, oracle.addr, {}, null, true),
  )
    .andThen((response: ApplyInputsResponse) =>
      cardanoCli.signTxEnvelope(oracle.skeyFile, response.tx).map(signed => ({
        contractId: response.contractId,
        txEnvelope: signed,
      })),
    )
    .andThen(({ txEnvelope }) => cardanoCli.submitTxEnvelope(txEnvelope).map(() => contractId))
    .andThen(contractIdAfter =>
      waitPatientlyForResultAsync(
        () => toAsync(marloweRuntimeCli.runGet(contractIdAfter, {}, null, true)),
        (state: ContractState) =>
          state.contractId === contractId &&
          state.state === null &&
          state.currentContract === null,
        { timeoutMs: POLL_TIMEOUT_MS, everyMs: POLL_EVERY_MS },
      ),
    );
};

// Orchestrates a full bet lifecycle:
//   1. init the contract (faucet pays for creation)
//   2. party1 deposits
//   3. party2 deposits
//   4. oracle submits the winning choice
//
// On success the final (closed) contract state is logged. On failure the
// underlying error is surfaced and re-thrown so the calling test can react.
export const run = async (opts: RunOpts): Promise<void> => {
  const { amount, party1, party2, oracle, faucet, winningChoice } = opts;
  const result = await initBetContract({
      amount,
      party1Addr: party1.addr,
      party2Addr: party2.addr,
      oracleAddr: oracle.addr,
      faucet,
    })
    .andThen(initialState => makeDeposit({ contractId: initialState.contractId, party: party1, amount }))
    .andThen(stateAfterParty1 => makeDeposit({ contractId: stateAfterParty1.contractId, party: party2, amount }))
    .andThen(stateAfterParty2 =>
      applyChoice({
        contractId: stateAfterParty2.contractId,
        oracle,
        choiceValue: WinningChoice.toChoiceValue(winningChoice),
      }),
    );

  result.match(
    (finalState: ContractState) => {
      console.log("Final contract state:", finalState);
    },
    (error: unknown) => {
      if (typeof error === "object" && error !== null && "stderr" in error) {
        console.error((error as { stderr: unknown }).stderr);
      } else {
        console.error(error);
      }
      throw new Error(`Bet run failed: ${String(error)}`);
    },
  );
};
