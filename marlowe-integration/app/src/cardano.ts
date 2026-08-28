import type { Tagged } from "type-fest";

// We use type level tagging across the project.
// Please avoid using `as` casting at all cost.
// Types which are tagged carry with them usually some
// invariants enforced either by the "smart contructor"
// or by the source of the data.
export type TxBodyHex = Tagged<string, "TxBodyHex">;
export type TxHex = Tagged<string, "TxHex">;
export type WitnessSetHex = Tagged<string, "WitnessSetHex">;
