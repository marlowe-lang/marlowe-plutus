import type { Json } from "@konduit/codec/json";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import * as codec from "@konduit/codec";
import { ContractId, MarloweVersion, TxEnvelope } from "./core";
import type { Tagged } from "type-fest";
import type { PolicyId } from "@konduit/konduit-consumer/cardano";

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

// data ContractState = ContractState
//   { contractId :: ContractId
//   , roleTokenMintingPolicyId :: PolicyId
//   , version :: MarloweVersion
//   , tags :: Map Text Metadata
//   , metadata :: Map Word64 Metadata
//   , status :: TxStatus
//   , block :: Maybe BlockHeader
//   , initialContract :: Semantics.Contract
//   , initialState :: Semantics.State
//   , currentContract :: Maybe Semantics.Contract
//   , state :: Maybe Semantics.State
//   , utxo :: Maybe TxOutRef
//   , assets :: Assets
//   , txBody :: Maybe TextEnvelope
//   , unclaimedPayouts :: [Payout]
//   }
// 
// FIXME: Turn all the below `Json` types into proper
// types and codecs.
export type ContractState = {
  assets: Json;
  block: Json | null;
  contractId: ContractId;
  currentContract: Json | null;
  initialContract: Json;
  initialState: Json;
  // metadata: Map<bigint, Json>;
  // roleTokenMintingPolicyId: PolicyId;
  state: Json | null;
  // status: string;
  // tags: { [key: string]: Json };
  // txBody?: TxEnvelope;
  // unclaimedPayouts: Payout[];
  // utxo?: TxOutRef;
  version: string;
}

export namespace ContractState {
  export const jsonCodec: jsonCodecs.JsonCodec<ContractState> = jsonCodecs.objectOf({
    assets: jsonCodecs.identityCodec,
    block: jsonCodecs.nullable(jsonCodecs.identityCodec),
    contractId: ContractId.jsonCodec,
    currentContract: jsonCodecs.nullable(jsonCodecs.identityCodec),
    initialContract: jsonCodecs.identityCodec,
    initialState: jsonCodecs.identityCodec,
    state: jsonCodecs.nullable(jsonCodecs.identityCodec),
    version: MarloweVersion.jsonCodec,
  })
}
