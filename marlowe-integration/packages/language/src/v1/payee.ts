import type { Party } from "./participants.js";

export type AccountId = Party;

export interface PayeeAccount {
  account: AccountId;
}

export interface PayeeParty {
  party: Party;
}

export type Payee = PayeeAccount | PayeeParty;

export type PayeeMatcher<T> = {
  party: (party: Party) => T;
  account: (account: AccountId) => T;
};

export function matchPayee<T>(matcher: PayeeMatcher<T>): (payee: Payee) => T;
export function matchPayee<T>(matcher: Partial<PayeeMatcher<T>>): (payee: Payee) => T | undefined;
export function matchPayee<T>(matcher: Partial<PayeeMatcher<T>>) {
  return (payee: Payee) => {
    if ("party" in payee && matcher.party) {
      return matcher.party(payee.party);
    } else if ("account" in payee && matcher.account) {
      return matcher.account(payee.account);
    }
  };
}