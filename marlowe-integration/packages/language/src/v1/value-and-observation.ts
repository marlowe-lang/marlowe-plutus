import type { ChoiceId } from "./choices.js";
import type { AccountId } from "./payee.js";
import type { Token } from "./token.js";

export interface AvailableMoney {
  amount_of_token: Token;
  in_account: AccountId;
}

export const constant = (constant: bigint) => constant;

export type Constant = bigint;

export type TimeIntervalStart = "time_interval_start";

export type TimeIntervalEnd = "time_interval_end";

export interface NegValue {
  negate: Value;
}

export interface AddValue {
  add: Value;
  and: Value;
}

export interface SubValue {
  value: Value;
  minus: Value;
}

export const mulValue = (multiply: Value, times: Value) => ({
  multiply: multiply,
  times: times,
});

export interface MulValue {
  multiply: Value;
  times: Value;
}

export interface DivValue {
  divide: Value;
  by: Value;
}

export type ChoiceValue = { value_of_choice: ChoiceId };

export type ValueId = string;

export interface UseValue {
  use_value: ValueId;
}

export interface Cond {
  if: Observation;
  then: Value;
  else: Value;
}

export type Value =
  | AvailableMoney
  | Constant
  | NegValue
  | AddValue
  | SubValue
  | MulValue
  | DivValue
  | ChoiceValue
  | TimeIntervalStart
  | TimeIntervalEnd
  | UseValue
  | Cond;

export interface AndObs {
  both: Observation;
  and: Observation;
}

export interface OrObs {
  either: Observation;
  or: Observation;
}

export interface NotObs {
  not: Observation;
}

export interface ChoseSomething {
  chose_something_for: ChoiceId;
}

export interface ValueEQ {
  value: Value;
  equal_to: Value;
}

export interface ValueGT {
  value: Value;
  gt: Value;
}

export interface ValueGE {
  value: Value;
  ge_than: Value;
}

export interface ValueLT {
  value: Value;
  lt: Value;
}

export interface ValueLE {
  value: Value;
  le_than: Value;
}

export type Observation =
  | AndObs
  | OrObs
  | NotObs
  | ChoseSomething
  | ValueEQ
  | ValueGT
  | ValueGE
  | ValueLT
  | ValueLE
  | boolean;