import * as codec from "@konduit/codec";
import type { Json } from "@konduit/codec/json";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import { err, ok } from "neverthrow";
import type { Tagged } from "type-fest";
import * as cardano from "@konduit/konduit-consumer/cardano";

export type CaseIndex = number;

export namespace CaseIndex {
  export const jsonCodec: jsonCodecs.JsonCodec<CaseIndex> = jsonCodecs.json2NumberCodec;
}

export type IsMerkleizedContinuation = boolean;

export namespace IsMerkleizedContinuation {
  export const jsonCodec: jsonCodecs.JsonCodec<IsMerkleizedContinuation> =
    jsonCodecs.json2BooleanCodec;
}

// An inclusive range of values for a choice.
export type Bound = { from: number; to: number };

export namespace Bound {
  export const jsonCodec: jsonCodecs.JsonCodec<Bound> = jsonCodecs.objectOf({
    from: jsonCodecs.json2NumberCodec,
    to: jsonCodecs.json2NumberCodec,
  });
}

// A token with a currency symbol (minting policy ID) and token name.
export type Token = { currency_symbol: string; token_name: string };

export namespace Token {
  export const jsonCodec: jsonCodecs.JsonCodec<Token> = jsonCodecs.objectOf({
    currency_symbol: jsonCodecs.json2StringCodec,
    token_name: jsonCodecs.json2StringCodec,
  });
}

// A participant in a contract, referred to either by a role token or a Cardano address.
export type Party = { role_token: string } | { address: cardano.AddressBech32 };

export namespace Party {
  const rolePartyCodec = jsonCodecs.objectOf({
    role_token: jsonCodecs.json2StringCodec,
  });

  const addressPartyCodec = jsonCodecs.objectOf({
    address: codec.rmap(
      jsonCodecs.json2StringCodec,
      (s) => s as unknown as cardano.AddressBech32,
      (a) => a as unknown as string,
    ),
  });

  export const jsonCodec: jsonCodecs.JsonCodec<Party> = {
    deserialise: (json) => {
      if (typeof json !== "object" || json === null || Array.isArray(json)) {
        return err("Expected a JSON object for Party");
      }
      const obj = json as Record<string, unknown>;
      if ("role_token" in obj) {
        return rolePartyCodec.deserialise(json as Json);
      }
      if ("address" in obj) {
        return addressPartyCodec.deserialise(json as Json);
      }
      return err(
        `Party must have either a role_token or address, got: ${JSON.stringify(json)}`,
      );
    },
    serialise: (party) => {
      if ("role_token" in party) {
        return rolePartyCodec.serialise(party);
      }
      return addressPartyCodec.serialise(party);
    },
  };
}

export type ChoiceId = { choice_name: string; choice_owner: Party };

export namespace ChoiceId {
  export const jsonCodec: jsonCodecs.JsonCodec<ChoiceId> = jsonCodecs.objectOf({
    choice_name: jsonCodecs.json2StringCodec,
    choice_owner: Party.jsonCodec,
  });
}

// Deposit Input that can be applied for a given contract.
export type CanDeposit = {
  party: Party;
  can_deposit: number;
  of_token: Token;
  into_account: Party;
  case_index: CaseIndex;
  is_merkleized_continuation: IsMerkleizedContinuation;
};

export namespace CanDeposit {
  export const jsonCodec: jsonCodecs.JsonCodec<CanDeposit> = jsonCodecs.objectOf({
    party: Party.jsonCodec,
    can_deposit: jsonCodecs.json2NumberCodec,
    of_token: Token.jsonCodec,
    into_account: Party.jsonCodec,
    case_index: CaseIndex.jsonCodec,
    is_merkleized_continuation: IsMerkleizedContinuation.jsonCodec,
  });
}

// Choice Inputs that can be applied for a given contract.
export type CanChoose = {
  for_choice: ChoiceId;
  can_choose_between: Bound[];
  case_index: CaseIndex;
  is_merkleized_continuation: IsMerkleizedContinuation;
};

export namespace CanChoose {
  export const jsonCodec: jsonCodecs.JsonCodec<CanChoose> = jsonCodecs.objectOf({
    for_choice: ChoiceId.jsonCodec,
    can_choose_between: jsonCodecs.arrayOf(Bound.jsonCodec),
    case_index: CaseIndex.jsonCodec,
    is_merkleized_continuation: IsMerkleizedContinuation.jsonCodec,
  });
}

// Notify Input that can be applied for a given contract.
export type CanNotify = {
  case_index: CaseIndex;
  is_merkleized_continuation: IsMerkleizedContinuation;
};

export namespace CanNotify {
  export const jsonCodec: jsonCodecs.JsonCodec<CanNotify> = jsonCodecs.objectOf({
    case_index: CaseIndex.jsonCodec,
    is_merkleized_continuation: IsMerkleizedContinuation.jsonCodec,
  });
}

// Applicable Inputs for a given contract.
export type ApplicableInputs = {
  deposits: CanDeposit[];
  choices: CanChoose[];
  notify?: CanNotify;
};

export namespace ApplicableInputs {
  const depositsCodec = jsonCodecs.arrayOf(CanDeposit.jsonCodec);
  const choicesCodec = jsonCodecs.arrayOf(CanChoose.jsonCodec);
  const notifyCodec = jsonCodecs.optional(CanNotify.jsonCodec);

  export const jsonCodec: jsonCodecs.JsonCodec<ApplicableInputs> = {
    deserialise: (json) => {
      if (typeof json !== "object" || json === null || Array.isArray(json)) {
        return err("Expected a JSON object for ApplicableInputs");
      }
      const obj = json as Record<string, unknown>;
      const deposits = depositsCodec.deserialise((obj.deposits ?? []) as Json);
      const choices = choicesCodec.deserialise((obj.choices ?? []) as Json);
      let notifyValue: CanNotify | undefined;
      let notifyErr: Json | undefined;
      if (obj.notify !== undefined && obj.notify !== null) {
        const n = notifyCodec.deserialise(obj.notify as Json);
        if (n.isErr()) {
          notifyErr = JSON.stringify(n.error) as Json;
        } else {
          notifyValue = n.value;
        }
      }
      if (deposits.isErr()) return err(deposits.error);
      if (choices.isErr()) return err(choices.error);
      if (notifyErr) return err(notifyErr);
      return ok<ApplicableInputs, Json>({ deposits: deposits.value, choices: choices.value, notify: notifyValue });
    },
    serialise: (value) => {
      const out: Record<string, Json> = {
        deposits: depositsCodec.serialise(value.deposits),
        choices: choicesCodec.serialise(value.choices),
      };
      if (value.notify !== undefined) {
        out.notify = notifyCodec.serialise(value.notify);
      }
      return out as Json;
    },
  };
}

// Indicates if a given contract can be reduced (apply []) or not.
export type CanReduce = boolean;

export namespace CanReduce {
  export const jsonCodec: jsonCodecs.JsonCodec<CanReduce> = jsonCodecs.json2BooleanCodec;
}

// Describe the reducibility (Can be Reduced ?) and the applicability (Can Inputs be Applied ?)
// for a given contract.
export type NextRecord = {
  applicable_inputs: ApplicableInputs;
  can_reduce: CanReduce;
};

export type Next = Tagged<NextRecord, "Next">;

export namespace Next {
  export const unsafeFromRecord = (record: NextRecord): Next => record as Next;
  export const jsonCodec: jsonCodecs.JsonCodec<Next> = codec.rmap(
    jsonCodecs.objectOf({
      applicable_inputs: ApplicableInputs.jsonCodec,
      can_reduce: CanReduce.jsonCodec,
    }),
    unsafeFromRecord,
    (next) => next,
  );
}