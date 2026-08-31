/**
 * This module is the main entrypoint for the language-core-v1 package. It offers static types and utility functions to work with
 * the {@link https://github.com/input-output-hk/marlowe/releases/download/v3/Marlowe.pdf | Marlowe Core Specification}.
 * The static types can only be used with TypeScript, but we offer the {@link @marlowe.io/language-core-v1!guards} module to check
 * at runtime if an object has the expected shape (which is useful for both JavaScript and TypeScript).

```
  import { Contract, Party, Value, lovelace, datetoTimeout } from "@marlowe.io/language-core-v1"

  // 1 Ada is equal to 1 Million lovelaces. The `n` at the end of the number is JavaScript way of
  // using bigint. Marlowe uses bigint to define Constant values.
  const oneADA: Value = 1000000n;

  // Other Marlowe datatypes are encoded as plain JSON objects as defined in the Appendix E of the Marlowe
  // Specification. For example, to express a MulValue object (multiplication), you need to define the
  // following JSON object
  const tenADA: Value = { multiply: 10n, times: oneADA };

  // Note that the explicit `: Value` type annotation is not strictly needed, the following line would
  // also work both in TypeScript and JavaScript
  // const tenADA = { multiply: 10n, times: oneADA};

  // The only difference is when we make a mistake. When we add the explicit annotation we inmediatly
  // get a compiler error close to the problematic code.

  // Try to modify "role_token" for "rle_token" in these expressions to see the difference.
  const bob: Party = { "role_token": "Bob" };
  const alice = { "role_token": "Alice" };

  const contract: Contract = {
    "when": [
      {
        "then": "close",
        "case": {
          "party": bob,
          "of_token": lovelace,
          "into_account": alice,
          "deposits": tenADA
        }
      }
    ],
    "timeout_continuation": "close",
    "timeout": datetoTimeout(new Date("2024-05-22"))
  }
```
 * @packageDocumentation
 */

export type { Action, Deposit, Notify, Choice } from "./actions.js";
export type { ChoiceName, ChoiceId, Bound, ChosenNum } from "./choices.js";
export type {
  Close,
  Pay,
  If,
  Let,
  Assert,
  Contract,
  When,
  Case,
  NormalCase,
  MerkleizedCase,
  Timeout,
} from "./contract.js";
export { close, datetoTimeout, timeoutToDate } from "./contract.js";
export type { Environment, TimeInterval } from "./environment.js";
export { mkEnvironment } from "./environment.js";

export type {
  Input,
  IDeposit,
  IChoice,
  INotify,
  BuiltinByteString,
  InputContent,
  NormalInput,
  MerkleizedInput,
  MerkleizedDeposit,
  MerkleizedChoice,
  MerkleizedHashAndContinuation,
  MerkleizedNotify,
} from "./inputs.js";
export { inputNotify } from "./inputs.js";

export type { Party, Address, Role, RoleName } from "./participants.js";
export { role, partiesToStrings, partyToString } from "./participants.js";

export type { Payee, PayeeAccount, PayeeParty, AccountId } from "./payee.js";

export type { Token, TokenName } from "./token.js";
export { tokenToString, token, adaToken, lovelace } from "./token.js";

export type { Accounts, MarloweState } from "./state.js";

export type {
  Value,
  ValueId,
  AvailableMoney,
  Constant,
  NegValue,
  AddValue,
  SubValue,
  MulValue,
  DivValue,
  ChoiceValue,
  TimeIntervalStart,
  TimeIntervalEnd,
  UseValue,
  Cond,
  Observation,
  AndObs,
  OrObs,
  NotObs,
  ChoseSomething,
  ValueEQ,
  ValueGT,
  ValueGE,
  ValueLT,
  ValueLE,
} from "./value-and-observation.js";

export type { TokenValue } from "./tokenValue.js";
export { tokenValue, adaValue } from "./tokenValue.js";
export type { PolicyId } from "./policyId.js";