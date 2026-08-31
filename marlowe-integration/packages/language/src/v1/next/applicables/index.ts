import type { CanDeposit } from "./canDeposit.js";
import type { CanChoose } from "./canChoose.js";
import type { CanNotify } from "./canNotify.js";

export interface ApplicableInputs {
  notify: CanNotify | null;
  deposits: CanDeposit[];
  choices: CanChoose[];
}