import * as codec from "@konduit/codec";
import * as jsonCodecs from "@konduit/codec/json/codecs";
import * as uint8Array from "@konduit/codec/uint8Array";
import type { Tagged } from "type-fest";
import * as cardano from "@konduit/konduit-consumer/cardano";
import { err, ok, Result } from "neverthrow";

export type ContractId = Tagged<string, "ContractId">;

export namespace ContractId {
  export const jsonCodec = codec.rmap(
    jsonCodecs.json2StringCodec,
    (contractIdStr) => contractIdStr as ContractId,
    (contractId) => contractId as string
 );
}

export type TxEnvelope = {
  tx: cardano.TxCborBytes;
  description: string;
  era: cardano.Era;
};

export namespace TxEnvelope {
  export const jsonCodec: jsonCodecs.JsonCodec<TxEnvelope> = (() => {
    const txEraTag2EraStr: codec.Codec<string, string, string> = {
      deserialise: (str: string): Result<string, string> => {
        const match = str.match(/^Tx (\w+)Era$/);
        if (match) {
          return ok(match[1]);
        }
        return err(`Invalid tx era tag: ${str}`);
      },
      serialise: (eraStr: string): string => `Tx ${eraStr}Era`,
    };
    return codec.rmap(jsonCodecs.objectOf({
        cborHex: codec.rmap(
          uint8Array.jsonCodec,
          cardano.TxCborBytes.unsafeFromBytes,
          (txCborBytes) => txCborBytes
        ),
        description: jsonCodecs.json2StringCodec,
        type: codec.pipe(
          jsonCodecs.json2StringCodec,
          codec.pipe(
            txEraTag2EraStr,
            cardano.Era.stringCodec,
          )
        )
      }),
      (json) => ({
        tx: json.cborHex,
        description: json.description,
        era: json.type,
      }),
      (txEnvelope) => ({
        cborHex: txEnvelope.tx,
        description: txEnvelope.description,
        type: txEnvelope.era,
      })
    );
  })();
}

