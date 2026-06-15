import type { Observation, Value, ValueId } from "./value-and-observation.js";
import type { AccountId, Payee } from "./payee.js";
import type { Token } from "./token.js";
import type { Action } from "./actions.js";

export const close = "close";

export type Close = "close";

export const pay = (pay: Value, token: Token, from_account: AccountId, to: Payee, then: Contract) => ({
  pay: pay,
  token: token,
  from_account: from_account,
  to: to,
  then: then,
});

export interface Pay {
  pay: Value;
  token: Token;
  from_account: AccountId;
  to: Payee;
  then: Contract;
}

export interface If {
  if: Observation;
  then: Contract;
  else: Contract;
}

export interface Let {
  let: ValueId;
  be: Value;
  then: Contract;
}

export interface Assert {
  assert: Observation;
  then: Contract;
}

export interface When {
  when: Case[];
  timeout: Timeout;
  timeout_continuation: Contract;
}

export interface NormalCase {
  case: Action;
  then: Contract;
}

export interface MerkleizedCase {
  case: Action;
  merkleized_then: string;
}

export type Case = NormalCase | MerkleizedCase;

export type Timeout = bigint;

export const datetoTimeout = (date: Date): Timeout =>
  BigInt(Math.floor(date.getTime() / 1000) * 1000);

export const timeoutToDate = (timeout: Timeout): Date => new Date(Number(timeout));

export type Contract = Close | Pay | If | When | Let | Assert;