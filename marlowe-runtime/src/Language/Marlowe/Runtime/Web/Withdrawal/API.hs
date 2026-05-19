{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Withdrawal.API (
  WithdrawalHeader (..),
  Withdrawal (..),
  WithdrawalsAPI,
  GetWithdrawalsResponse,
  PostWithdrawalsRequest (..),
  PostWithdrawalsResponse,
  WithdrawalAPI,
  GetWithdrawalAPI,
  PostTransactionsRequest (..),
) where

import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedResponse)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema, ListObject,
 )
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Language.Marlowe.Runtime.Web.Core.Asset (PolicyId)

import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Servant (
  Capture,
  Description,
  JSON,
  QueryParams,
  ReqBody,
  Summary,
  type (:<|>),
  type (:>), StdMethod (POST, GET), UVerb, Header, HasStatus, StatusOf
 )

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (
  ToSchema,
 )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant.Pagination (HasPagination (..), Ranges)

import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TxId,
  TxOutRef,
  TxStatus,
 )
import Language.Marlowe.Runtime.Web.Payout.API (PayoutHeader)
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  PostTxAPI,
  TxJSON,
  WithdrawTx,
  WithdrawTxEnvelope,
 )

-- | /withdrawals sub-API
type WithdrawalsAPI =
  GetWithdrawalsAPI
    :<|> PostWithdrawalsAPI
    :<|> Capture "withdrawalId" TxId :> WithdrawalAPI

-- | /contracts/:contractId/withdrawals/:withdrawalId sub-API
type WithdrawalAPI =
  GetWithdrawalAPI
    -- :<|> Summary "Submit payout withdrawal"
    --   :> Description
    --       "Submit a signed (Cardano) transaction that withdraws available payouts from a role payout validator. \
    --       \The transaction must have originally been created by the POST /withdrawals endpoint. \
    --       \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
    --       \it will not wait for the transaction to be published in a block. \
    --       \Use the GET /withdrawals/{withdrawalId} endpoint to poll the on-chain status."
    --   :> OperationId "submitWithdrawal"
    --   :> PutSignedTxAPI

-- | GET /contracts/:contractId/withdrawals/:withdrawalId sub-API
type GetWithdrawalAPI =
  Summary "Get withdrawal by ID"
    :>  OperationId "getWithdrawalById"
    :>  UVerb
          'GET
          '[JSON]
          '[Withdrawal]

-- | POST /contracts sub-API
type PostWithdrawalsAPI =
  Summary "Withdraw payouts"
    :> Description
        "Build an unsigned (Cardano) transaction body which withdraws available payouts from a role payout validator. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /withdrawals/{withdrawalId} endpoint."
    :> OperationId "withdrawPayouts"
    :> RenameResponseSchema "WithdrawPayoutsResponse"
    :> ReqBody '[JSON] PostWithdrawalsRequest
    :> PostTxAPI
      ( UVerb
          'POST
          '[TxJSON WithdrawTx]
          '[PostWithdrawalsResponse CardanoTx]
      )

type PostWithdrawalsResponse tx = WithLink "withdrawal" (WithdrawTxEnvelope tx)

instance HasStatus (PostWithdrawalsResponse CardanoTx) where
  type StatusOf (PostWithdrawalsResponse CardanoTx) = 201

-- | GET /contracts/:contractId/withdrawals sub-API
type GetWithdrawalsAPI =
  Summary "Get withdrawals"
    :> Description
        "Get published withdrawal transactions. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getWithdrawals"
    :> QueryParams "roleCurrency" PolicyId
    :> RenameResponseSchema "GetWithdrawalsResponse"
    -- :> PaginatedGet '["withdrawalId"] GetWithdrawalsResponse
    :> Header "Range" (Ranges '["withdrawalId"] GetWithdrawalsResponse)
    :> UVerb
        'GET
        '[JSON]
        '[PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse]

type GetWithdrawalsResponse = WithLink "withdrawal" WithdrawalHeader

instance HasStatus (ListObject GetWithdrawalsResponse) where
  type StatusOf (ListObject GetWithdrawalsResponse) = 206

data WithdrawalHeader = WithdrawalHeader
  { withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance HasPagination WithdrawalHeader "withdrawalId" where
  type RangeType WithdrawalHeader "withdrawalId" = TxId
  getFieldValue _ WithdrawalHeader{..} = withdrawalId

data Withdrawal = Withdrawal
  { payouts :: Set PayoutHeader
  , withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance HasStatus Withdrawal where
  type StatusOf Withdrawal = 200

newtype PostWithdrawalsRequest = PostWithdrawalsRequest
  { payouts :: Set TxOutRef
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PostWithdrawalsRequest
instance ToJSON PostWithdrawalsRequest
instance ToSchema PostWithdrawalsRequest

data PostTransactionsRequest = PostTransactionsRequest
  { version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , invalidBefore :: Maybe UTCTime
  , invalidHereafter :: Maybe UTCTime
  , inputs :: [Semantics.Input]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
