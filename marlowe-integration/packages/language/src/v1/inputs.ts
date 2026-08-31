import type { Contract } from "./contract.js";
import type { ChoiceId, ChosenNum } from "./choices.js";
import type { Party } from "./participants.js";
import type { AccountId } from "./payee.js";
import type { Token } from "./token.js";

export interface IChoice {
  for_choice_id: ChoiceId;
  input_that_chooses_num: ChosenNum;
}

export interface IDeposit {
  input_from_party: Party;
  that_deposits: bigint;
  of_token: Token;
  into_account: AccountId;
}

export const inputNotify = "input_notify";

export type INotify = "input_notify";

export type BuiltinByteString = string;

export type InputContent = IDeposit | IChoice | INotify;

export type NormalInput = InputContent;

export interface MerkleizedHashAndContinuation {
  continuation_hash: BuiltinByteString;
  merkleized_continuation: Contract;
}

export type MerkleizedDeposit = IDeposit & MerkleizedHashAndContinuation;

export type MerkleizedChoice = IChoice & MerkleizedHashAndContinuation;

export type MerkleizedNotify = MerkleizedHashAndContinuation;

export type MerkleizedInput = MerkleizedDeposit | MerkleizedChoice | MerkleizedNotify;

export type Input = NormalInput | MerkleizedInput;
