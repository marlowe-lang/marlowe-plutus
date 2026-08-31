import type { Contract } from "./contract.js";
import type { TimeInterval } from "./environment.js";
import type { Input } from "./inputs.js";
import type { Party } from "./participants.js";
import type { AccountId, Payee } from "./payee.js";
import type { MarloweState } from "./state.js";
import type { Token } from "./token.js";

export interface Payment {
  payment_from: AccountId;
  to: Payee;
  amount: bigint;
  token: Token;
}

export interface Transaction {
  tx_interval: TimeInterval;
  tx_inputs: Input[];
}

export interface SingleInputTx {
  interval: TimeInterval;
  input?: Input;
}

export interface NonPositiveDeposit {
  party: Party;
  asked_to_deposit: bigint;
  of_token: Token;
  in_account: AccountId;
}

export interface NonPositivePay {
  account: AccountId;
  asked_to_pay: bigint;
  of_token: Token;
  to_payee: Payee;
}

export interface PartialPay {
  account: AccountId;
  asked_to_pay: bigint;
  of_token: Token;
  to_payee: Payee;
  but_only_paid: bigint;
}

export interface Shadowing {
  value_id: string;
  had_value: bigint;
  is_now_assigned: bigint;
}

export type AssertionFailed = "assertion_failed";

export type TransactionWarning = NonPositiveDeposit | NonPositivePay | PartialPay | Shadowing | AssertionFailed;

export interface InvalidInterval {
  invalidInterval: { from: bigint; to: bigint };
}

export interface IntervalInPast {
  intervalInPastError: { from: bigint; to: bigint; minTime: bigint };
}

export type IntervalError = InvalidInterval | IntervalInPast;

export type AmbiguousTimeIntervalError = "TEAmbiguousTimeIntervalError";

export type ApplyNoMatchError = "TEApplyNoMatchError";

export type UselessTransaction = "TEUselessTransaction";

export type HashMismatchError = "TEHashMismatch";

export interface TEIntervalError {
  error: "TEIntervalError";
  context: IntervalError;
}

export type TransactionError =
  | AmbiguousTimeIntervalError
  | ApplyNoMatchError
  | UselessTransaction
  | TEIntervalError
  | HashMismatchError;

export interface TransactionSuccess {
  warnings: TransactionWarning[];
  payments: Payment[];
  state: MarloweState;
  contract: Contract;
}

export type TransactionOutput = TransactionSuccess | { transaction_error: TransactionError };