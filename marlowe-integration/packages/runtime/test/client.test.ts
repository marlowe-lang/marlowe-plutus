import { describe, it } from 'vitest';
import { mkParser } from '@konduit/codec/json/codecs';
import { ContractState } from '../src/client';

describe('Client codecs', () => {
  describe('Contract codecs', () => {
    describe('Contract state', () => {
      let contractStateParser = mkParser(ContractState.jsonCodec);
      describe('On-chain contract state', () => {
        // Test with all fields present
        it('Should be decoded correctly', () => {
          contractStateParser(onChainContractStateJSON).mapErr(
            (err) => {
              console.error('Error parsing contract state JSON:', err);
              throw err;
            }
          );
        });
      });
    })
  });
});

// data ContractState = ContractState
//   { assets :: Assets
//   , block :: Maybe BlockHeader
//   , contractId :: ContractId
//   , currentContract :: Maybe Semantics.Contract
//   , initialContract :: Semantics.Contract
//   , initialState :: Semantics.State
//   , metadata :: Map Word64 Metadata
//   , roleTokenMintingPolicyId :: PolicyId
//   , state :: Maybe Semantics.State
//   , status :: TxStatus
//   , tags :: Map Text Metadata
//   , txBody :: Maybe TextEnvelope
//   , unclaimedPayouts :: [Payout]
//   , utxo :: Maybe TxOutRef
//   , version :: MarloweVersion
//   }
// 
const onChainContractStateJSON: string = `
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
    "roleTokenMintingPolicyId": null,
    "status": "confirmed",
    "tags": {},
    "txBody": null,
    "unclaimedPayouts": [],
    "utxo": "6beb89c75519e58ff985de1a9170e90d35f571481bae48e0452b0ca09911c008#1",
    "version": "v1"
  }
`;

