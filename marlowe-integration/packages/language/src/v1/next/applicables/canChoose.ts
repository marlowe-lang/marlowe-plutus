import type { ChosenNum, ChoiceId } from "../../choices.js";
import type { IChoice } from "../../inputs.js";

export interface CanChoose {
  case_index: bigint;
  for_choice: ChoiceId;
  can_choose_between: { from: bigint; to: bigint }[];
  is_merkleized_continuation: boolean;
}

export const toInput: (canChoose: CanChoose) => (chosenNum: ChosenNum) => IChoice = (canChoose) => (chosenNum) => ({
  for_choice_id: canChoose.for_choice,
  input_that_chooses_num: chosenNum,
});