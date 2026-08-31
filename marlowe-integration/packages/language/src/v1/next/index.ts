export interface Next {
  can_reduce: boolean;
  applicable_inputs: ApplicableInputs;
}

import type { ApplicableInputs } from "./applicables/index.js";

export type { CanDeposit } from "./applicables/canDeposit.js";
export type { CanChoose } from "./applicables/canChoose.js";
export type { CanNotify } from "./applicables/canNotify.js";

export const emptyApplicables = (next: Next) => {
  return (
    next.applicable_inputs.choices.length === 0 &&
    next.applicable_inputs.deposits.length === 0 &&
    next.applicable_inputs.notify === null
  );
};

export const noNext: Next = {
  can_reduce: false,
  applicable_inputs: {
    deposits: [],
    choices: [],
    notify: null,
  },
};