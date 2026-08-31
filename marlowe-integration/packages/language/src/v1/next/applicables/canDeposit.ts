import type { Token } from "../../token.js";
import type { IDeposit } from "../../inputs.js";
import type { Party } from "../../participants.js";
import type { AccountId } from "../../payee.js";

export interface CanDeposit {
  case_index: bigint;
  party: Party;
  can_deposit: bigint;
  of_token: Token;
  into_account: AccountId;
  is_merkleized_continuation: boolean;
}

export const toInput: (canDeposit: CanDeposit) => IDeposit = (canDeposit) => ({
  input_from_party: canDeposit.party,
  that_deposits: canDeposit.can_deposit,
  of_token: canDeposit.of_token,
  into_account: canDeposit.into_account,
});