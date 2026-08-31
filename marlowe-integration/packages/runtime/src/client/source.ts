import * as codec from "@konduit/codec";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import type { Tagged } from "type-fest";

// FIXME: paluh: add proper json codecs for `Contract` once the runtime client
// exposes one. For now we operate on plain JSON shapes for source responses.

// Hex-encoded 32-byte identifier of a Marlowe contract source.
export type ContractSourceId = Tagged<string, "ContractSourceId">;

export namespace ContractSourceId {
  export const jsonCodec: jsonCodecs.JsonCodec<ContractSourceId> = codec.rmap(
    jsonCodecs.json2StringCodec,
    (idStr) => idStr as ContractSourceId,
    (id) => id as string,
  );
}

export type PostContractSourceResponseRecord = {
  contractSourceId: ContractSourceId;
  intermediateIds: { [label: string]: ContractSourceId };
};

export type PostContractSourceResponse = Tagged<
  PostContractSourceResponseRecord,
  "PostContractSourceResponse"
>;

export namespace PostContractSourceResponse {
  export const jsonCodec: jsonCodecs.JsonCodec<PostContractSourceResponse> =
    codec.rmap(
      jsonCodecs.objectOf({
        contractSourceId: ContractSourceId.jsonCodec,
        intermediateIds: jsonCodecs.dictOf(ContractSourceId.jsonCodec),
      }),
      (record) => record as PostContractSourceResponse,
      (response) => response,
    );
}