/**
 * This module defines the participant types as defined in the Marlowe Specification.
 * @see Section 2.1.1 of the {@link https://github.com/input-output-hk/marlowe/releases/download/v3/specification-v3.pdf | Marlowe Specification}
 * @packageDocumentation
 */
import type { AddressBech32 } from "./address.js";
import type { Sort } from "../assoc-map.js";

export interface Address {
  address: AddressBech32;
}

export type RoleName = string;

export const role = (roleToken: RoleName) => ({ role_token: roleToken });

export interface Role {
  role_token: RoleName;
}

export const party = (party: Role | Address) => party;

export type Party = Address | Role;

export const partiesToStrings: (parties: Party[]) => string[] = (parties) =>
  parties.map(partyToString);

export const partyToString: (party: Party) => string = (party) =>
  "role_token" in party ? party.role_token : party.address;

export function partyCmp(a: Party, b: Party): Sort {
  if ("role_token" in a && !("role_token" in b)) {
    return "LowerThan";
  }
  if (!("role_token" in a) && "role_token" in b) {
    return "GreaterThan";
  }
  if ("address" in a && "address" in b) {
    return strCmp(a.address, b.address);
  }
  if ("role_token" in a && "role_token" in b) {
    return strCmp(a.role_token, b.role_token);
  }
  throw new Error("Unreachable");
}

function strCmp(a: string, b: string): Sort {
  if (a < b) {
    return "LowerThan";
  }
  if (a > b) {
    return "GreaterThan";
  }
  return "EqualTo";
}
