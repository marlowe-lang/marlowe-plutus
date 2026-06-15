import { describe, it, expect } from 'vitest';
import { parse, stringify, type Json } from '../src/json';
import { json2StringCodec, json2BooleanCodec, json2NullCodec, objectOf, altJsonCodecs, optional, type JsonCodec, arrayOf, tupleOf, json2NumberCodec, json2BigIntCodec } from '../src/json/codecs';
import * as json from '../src/json';
import { unwrapOk, unwrapErr } from './assertions';
import { NonNegativeInt } from '../src/integers/smallish';

describe('Client codecs', () => {
  describe('contract codecs', () => {
    // it('should handle null json correctly', () => {
    //   expect(json.nullJson).toBeNull();
    //   const encoded = null;
    //   expect(encoded).toBeNull();
    //   const decoded = unwrapOk(json2NullCodec.deserialise(encoded));
    //   expect(decoded).toBeNull();
    // });

    describe('contract details', () => {
      type Person = {
        name: string;
        nickname: null | string;
        age: number | undefined;
      };
      const json2personCodec: JsonCodec<Person> = objectOf({
        name: json2StringCodec,
        nickname: altJsonCodecs(
          [json2NullCodec, json2StringCodec],
          (serNull, serString) => (value: null | string) =>
            value === null ? serNull(null) : serString(value)
        ),
        age: optional(json2NumberCodec)
      });

      // Test with all fields present
      it('should encode and decode person with all fields', () => {
        const fullPerson: Person = {
          name: "Alice",
          nickname: "Ally",
          age: 30
        };
        const encoded1 = json2personCodec.serialise(fullPerson);
        const decoded1 = unwrapOk(json2personCodec.deserialise(encoded1));
        expect(decoded1).toEqual(fullPerson);
      });
    })
  });
});

const onChainContractResponse: string = `
  {
    "assets": {
      "lovelace": 1008540,
      "tokens": {}
    },
    "block": {
      "blockHeaderHash": "2553ed60b8f81f297a35ecf0ce8e04fa21878af725ed7eab757efbbcad270b97",
      "blockNo": 1285,
      "slotNo": 12940
    },
    "contractId": "6beb89c75519e58ff985de1a9170e90d35f571481bae48e0452b0ca09911c008#1",
    "currentContract": {
      "timeout": 1781516552087,
      "timeout_continuation": "close",
      "when": [
        {
          "case": {
            "deposits": 12000000,
            "into_account": {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            },
            "of_token": {
              "currency_symbol": "",
              "token_name": ""
            },
            "party": {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            }
          },
          "then": "close"
        }
      ]
    },
    "initialContract": {
      "timeout": 1781516552087,
      "timeout_continuation": "close",
      "when": [
        {
          "case": {
            "deposits": 12000000,
            "into_account": {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            },
            "of_token": {
              "currency_symbol": "",
              "token_name": ""
            },
            "party": {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            }
          },
          "then": "close"
        }
      ]
    },
    "initialState": {
      "accounts": [
        [
          [
            {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            },
            {
              "currency_symbol": "",
              "token_name": ""
            }
          ],
          1008540
        ]
      ],
      "boundValues": [],
      "choices": [],
      "minTime": 0
    },
    "metadata": {},
    "roleTokenMintingPolicyId": "",
    "state": {
      "accounts": [
        [
          [
            {
              "address": "addr_test1vr4tkl87dqa0m0hd4xp7h2kcxpcnj98dma40lh8das0xyss8hrgvs"
            },
            {
              "currency_symbol": "",
              "token_name": ""
            }
          ],
          1008540
        ]
      ],
      "boundValues": [],
      "choices": [],
      "minTime": 0
    },
    "status": "confirmed",
    "tags": {},
    "txBody": null,
    "unclaimedPayouts": [],
    "utxo": "6beb89c75519e58ff985de1a9170e90d35f571481bae48e0452b0ca09911c008#1",
    "version": []
  }
`;

