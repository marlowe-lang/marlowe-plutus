{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  TransactionsAPI,
  GetTransactionsAPI,
  PostTransactionsAPI,
  PostTransactionsRequest(..),
  GetTransactionsResponse,
  TransactionAPI,
  GetTransactionAPI,
  GetTransactionResponse,
  PostTransactionsResponse,
) where

import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedResponse)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema, ListObject,
 )
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx (TxId)
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTx,
  ApplyInputsTxEnvelope,
  CardanoTx,
  PostTxAPI,
  Tx,
  TxHeader,
  TxJSON,
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (PostTransactionsRequest(PostTransactionsRequest, inputs, invalidBefore, invalidHereafter, metadata, tags, version))
import Servant (
  Capture,
  Description,
  JSON,
  ReqBody,
  Summary,
  type (:<|>),
  type (:>), UVerb, StdMethod (GET, POST), Header, HasStatus, StatusOf
 )
import Servant.Pagination (Ranges)

-- | /contracts/:contractId/transactions sub-API
type TransactionsAPI =
  GetTransactionsAPI
    :<|> PostTransactionsAPI
    :<|> Capture "transactionId" TxId :> TransactionAPI

-- | GET /contracts/:contractId/transactions sub-API
type GetTransactionsAPI =
  Summary "Get transactions for contract"
    :> Description
        "Get published transactions for a contract. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getTransactionsForContract"
    :> RenameResponseSchema "GetTransactionsResponse"
    :> Header "Range" (Ranges '["transactionId"] GetTransactionsResponse)
    :> UVerb
        'GET
        '[JSON]
        '[PaginatedResponse '["transactionId"] GetTransactionsResponse]

type GetTransactionsResponse = WithLink "transaction" TxHeader

instance HasStatus (ListObject GetTransactionsResponse) where
  type StatusOf (ListObject GetTransactionsResponse) = 206

-- | /contracts/:contractId/transactions/:transactionId sub-API
type TransactionAPI =
  GetTransactionAPI
--    :<|> Summary "Submit contract input application"
--      :> Description
--          "Submit a signed (Cardano) transaction that applies inputs to an open Marlowe contract. \
--          \The transaction must have originally been created by the POST /contracts/{contractId}/transactions endpoint. \
--          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
--          \it will not wait for the transaction to be published in a block. \
--          \Use the GET /contracts/{contractId}/transactions/{transactionId} endpoint to poll the on-chain status."
--      :> OperationId "submitContractTransaction"
--      :> PutSignedTxAPI

-- | GET /contracts/:contractId/transactions/:transactionId sub-API
type GetTransactionAPI =
  Summary "Get contract transaction by ID"
    :> OperationId "getContractTransactionById"
    :> RenameResponseSchema "GetTransactionResponse"
    :> UVerb
        'GET
        '[JSON]
        '[GetTransactionResponse]

type GetTransactionResponse = WithLink "previous" (WithLink "next" Tx)

instance HasStatus GetTransactionResponse where
  type StatusOf GetTransactionResponse = 200

-- | POST /contracts/:contractId/transactions sub-API
type PostTransactionsAPI =
  Summary "Apply inputs to contract"
    :> Description
        "Build an unsigned (Cardano) transaction body which applies inputs to an open Marlowe contract. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /contracts/{contractId}/transactions/{transactionId} endpoint."
    :> OperationId "applyInputsToContract"
    :> RenameResponseSchema "ApplyInputsResponse"
    :> ReqBody '[JSON] PostTransactionsRequest
    :> PostTxAPI
      ( UVerb
          'POST
          '[TxJSON ApplyInputsTx]
          '[PostTransactionsResponse CardanoTx]
      )

type PostTransactionsResponse tx = WithLink "transaction" (ApplyInputsTxEnvelope tx)

instance HasStatus (PostTransactionsResponse CardanoTx) where
  type StatusOf (PostTransactionsResponse CardanoTx) = 201
