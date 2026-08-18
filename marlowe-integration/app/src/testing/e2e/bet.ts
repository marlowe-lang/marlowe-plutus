import { AddressBech32 } from '@konduit/konduit-consumer/cardano';
import type { Contract, Observation, Value } from '@marlowe-lang/language/v1';
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
  const CHOICE_NAME = "team-1-vs-team-2";
  const NO_WINNERS = 0n;
  const TEAM_1_WINS = 1n;
  const TEAM_2_WINS = 2n;

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
