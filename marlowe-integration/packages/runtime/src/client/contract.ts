import type { Json } from "@konduit/codec/json";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import * as codec from "@konduit/codec";
import { ContractId, TxEnvelope } from "./core";
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
export type ContractState = {
  contractId: ContractId;
  // roleTokenMintingPolicyId: PolicyId;
  version: string;
  // tags: { [key: string]: Json };
  // metadata: Map<bigint, Json>;
  // status: string;
  // block?: Json;
  initialContract: Json;
  initialState: Json;
  currentContract?: Json;
  state?: Json;
  // utxo?: TxOutRef;
  // assets: Json;
  // txBody?: TxEnvelope;
  // unclaimedPayouts: Payout[];
}
