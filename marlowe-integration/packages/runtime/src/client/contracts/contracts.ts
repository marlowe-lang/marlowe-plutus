import type { Json } from "@konduit/codec/json";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import * as codec from "@konduit/codec";
import { ContractId, MarloweVersion, TxEnvelope } from "../core";
import type { Tagged } from "type-fest";
import * as cardano from "@konduit/konduit-consumer/cardano";

export type PostCreateContractResponseRecord = {
  contractId: ContractId;
  safetyErrors: Json[];
  tx: TxEnvelope;
};
export type PostCreateContractResponse = Tagged<PostCreateContractResponseRecord, "PostCreateContractResponse">;

export namespace PostCreateContractResponse {
  export const unsafeFromRecord = (record: PostCreateContractResponseRecord): PostCreateContractResponse => record as PostCreateContractResponse;
  export const jsonCodec: jsonCodecs.JsonCodec<PostCreateContractResponse> = codec.rmap(jsonCodecs.objectOf({
      contractId: ContractId.jsonCodec,
      safetyErrors: jsonCodecs.arrayOf(jsonCodecs.identityCodec),
      tx: TxEnvelope.jsonCodec,
    }),
    unsafeFromRecord,
    (response) => response
  )
}

// On the server side:
// data ApplyInputsTxEnvelope tx = ApplyInputsTxEnvelope
//   { contractId :: TxOutRef
//   , transactionId :: TxId
//   , txEnvelope :: TextEnvelope
//   , safetyErrors :: [SafetyError]
//   }
//   deriving (Show, Eq, Generic)

export type ApplyInputsResponse = {
  contractId: ContractId;
  transactionId: string; // FIXME: Use cardano.TxId.jsonCodec when available
  tx: TxEnvelope;
  safetyErrors: Json[]; // FIXME: Use SafetyError type if available
};

export namespace ApplyInputsResponse {
  export const jsonCodec: jsonCodecs.JsonCodec<ApplyInputsResponse> = jsonCodecs.objectOf({
    contractId: ContractId.jsonCodec,
    transactionId: jsonCodecs.json2StringCodec,
    tx: TxEnvelope.jsonCodec,
    safetyErrors: jsonCodecs.arrayOf(jsonCodecs.identityCodec),
  });
}

// FIXME: Every `Json` below is a placeholder.
export type ContractState = {
  assets: Json;
  block: Json | null;
  contractId: ContractId;
  currentContract: Json | null;
  initialContract: Json;
  initialState: Json;
  // metadata: Map<bigint, Json>;
  roleTokenMintingPolicyId: cardano.PolicyId | null;
  state: Json | null;
  // status: string;
  // tags: { [key: string]: Json };
  // txBody?: TxEnvelope;
  // unclaimedPayouts: Payout[];
  // utxo?: TxOutRef;
  version: MarloweVersion;
}

export namespace ContractState {
  export const jsonCodec: jsonCodecs.JsonCodec<ContractState> = jsonCodecs.objectOf({
    assets: jsonCodecs.identityCodec,
    block: jsonCodecs.nullable(jsonCodecs.identityCodec),
    contractId: ContractId.jsonCodec,
    currentContract: jsonCodecs.nullable(jsonCodecs.identityCodec),
    initialContract: jsonCodecs.identityCodec,
    initialState: jsonCodecs.identityCodec,
    roleTokenMintingPolicyId: jsonCodecs.nullable(cardano.PolicyId.jsonCodec),
    state: jsonCodecs.nullable(jsonCodecs.identityCodec),
    version: MarloweVersion.jsonCodec,
  })
}
