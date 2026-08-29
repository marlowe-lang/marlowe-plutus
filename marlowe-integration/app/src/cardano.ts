import type { Tagged } from "type-fest";
import type { AddressBech32 } from "@konduit/konduit-consumer/cardano";
import type { Path } from "./exec.js";

// We use type level tagging across the project.
// Please avoid using `as` casting at all cost.
// Types which are tagged carry with them usually some
// invariants enforced either by the "smart contructor"
// or by the source of the data.
export type TxBodyHex = Tagged<string, "TxBodyHex">;
export type TxHex = Tagged<string, "TxHex">;
export type WitnessSetHex = Tagged<string, "WitnessSetHex">;

// A wallet we own and can sign transactions on behalf of.
// Holds the address that identifies the wallet and the path to its signing key.
export type Wallet = {
  addr: AddressBech32;
  skeyFile: Path;
};
