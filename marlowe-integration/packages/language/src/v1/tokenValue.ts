import type { Token } from "./token.js";

export interface TokenValue {
  amount: bigint;
  token: Token;
}

export const tokenValue: (amount: bigint) => (token: Token) => TokenValue = (amount) => (token) => ({
  amount: amount,
  token: token,
});

export const lovelaceValue: (lovelaces: bigint) => TokenValue = (lovelaces) => ({
  amount: lovelaces,
  token: { currency_symbol: "", token_name: "" },
});

export const adaValue: (adaAmount: bigint) => TokenValue = (adaAmount) => ({
  amount: adaAmount * 1_000_000n,
  token: { currency_symbol: "", token_name: "" },
});