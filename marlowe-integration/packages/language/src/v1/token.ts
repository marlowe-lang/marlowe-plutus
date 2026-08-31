import type { Sort } from "../assoc-map.js";
import type { PolicyId } from "./policyId.js";

export type TokenName = string;

export interface Token {
  currency_symbol: PolicyId;
  token_name: TokenName;
}

export const token = (currency_symbol: PolicyId, token_name: TokenName) => ({
  currency_symbol: currency_symbol,
  token_name: token_name,
});

export const tokenToString: (token: Token) => string = (token) => `${token.currency_symbol}|${token.token_name}`;

export const lovelace: Token = token("", "");

export const adaToken: Token = lovelace;

export function tokenCmp(a: Token, b: Token): Sort {
  const currencyCmp = strCmp(a.currency_symbol, b.currency_symbol);
  if (currencyCmp !== "EqualTo") {
    return currencyCmp;
  }
  return strCmp(a.token_name, b.token_name);
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