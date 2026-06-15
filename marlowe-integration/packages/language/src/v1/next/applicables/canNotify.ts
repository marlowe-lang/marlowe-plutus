import type { INotify } from "../../inputs.js";

export interface CanNotify {
  case_index: bigint;
  is_merkleized_continuation: boolean;
}

export const toInput: (canNotify: CanNotify) => INotify = (canNotify) => "input_notify";