{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- FIXME: Drop this after the contract endpoint is finished.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Marlowe.Runtime.Web.Contract.API (
  ContractId,
  ContractHeader (..),
  ContractState (..),
  ContractsAPI,
  ContractAPI,
  GetContractAPI,
  GetContractResponse,
  ContractSourcesAPI,
  ContractSourceAPI,
  ContractSourceId(..),
  GetContractsAPI,
  GetContractsResponse,
  PostContractsRequest (..),
  PostContractsResponse,
  PostContractSourceResponse (..),
  PreserveActions (..),
  ContractOrSourceId (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet)
import Language.Marlowe.Runtime.Web.Adapter.Servant (OperationId, RenameResponseSchema)
import Language.Marlowe.Runtime.Web.Contract.Next.API (NextAPI)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Language.Marlowe.Runtime.Web.Core.Address (
  Address,
  StakeAddress,
 )
import Language.Marlowe.Runtime.Web.Core.Asset (
  AssetId,
  Assets,
  PolicyId,
 )
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (
  MarloweVersion,
 )
import Language.Marlowe.Runtime.Web.Core.Party (Party)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope,
  TxOutRef,
  TxStatus,
 )
import Language.Marlowe.Runtime.Web.Payout.API (Payout)

import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Roles (RolesConfig)
import Servant (
  Capture,
  Description,
  Header',
  JSON,
  Optional,
  QueryParams,
  ReqBody,
  Strict,
  Summary,
  type (:<|>),
  type (:>), HasStatus, StatusOf
 )
import Servant.API (UVerb, StdMethod (GET, POST))
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )

import Control.DeepSeq (NFData)
import Data.OpenApi (ToSchema (..))
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CreateTxEnvelope,
  PostTxAPI,
 )
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (TransactionsAPI)
import Language.Marlowe.Runtime.Web.Contract.Source.API (ContractSourcesAPI, ContractOrSourceId (..), ContractSourceAPI, ContractSourceId (..), PostContractSourceResponse (..), PreserveActions (..))

type ContractId = TxOutRef

type ContractsAPI =
  Capture "contractId" ContractId :> ContractAPI
  :<|> PostContractsAPI
  -- :<|> GetContractsAPI
  :<|> "sources" :> ContractSourcesAPI

-- | GET /contracts sub-API
type GetContractsAPI =
  Summary "Get contracts"
    :> Description
        "Get contracts published on chain. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getContracts"
    :> QueryParams "roleCurrency" PolicyId
    :> QueryParams "tag" Text
    :> QueryParams "partyAddress" Address
    :> QueryParams "partyRole" AssetId
    :> RenameResponseSchema "GetContractsResponse"
    :> PaginatedGet '["contractId"] GetContractsResponse

type GetContractsResponse = WithLink "transactions" (WithLink "contract" ContractHeader)

instance HasStatus GetContractsResponse where
  type StatusOf GetContractsResponse = 200

-- | POST /contracts sub-API
type PostContractsAPI =
  Summary "Create a new contract"
    :> Description
        "Build an unsigned (Cardano) transaction which opens a new Marlowe contract. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the wallet API - the current Runtime backend does not support submitting."
    :> OperationId "initContract"
    :> RenameResponseSchema "InitContractResponse"
    :> Header'
        '[Optional, Strict, Description "Where to send staking rewards for the Marlowe script outputs of this contract."]
        "X-Stake-Address"
        StakeAddress
    :> ReqBody '[JSON] PostContractsRequest
    :> PostTxAPI
        -- (PostCreated '[TxJSON ContractTx] (PostContractsResponse CardanoTx))
        ( UVerb
            'POST
            '[JSON]
            '[PostContractsResponse CardanoTx]
        )

-- | /contracts/:contractId sub-API
type ContractAPI =
  GetContractAPI
    :<|> "next" :> NextAPI
    :<|> "transactions" :> TransactionsAPI

--    :<|> Summary "Submit contract to chain"
--      :> Description
--          "Submit a signed (Cardano) transaction that opens a new Marlowe contract. \
--          \The transaction must have originally been created by the POST /contracts endpoint. \
--          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
--          \it will not wait for the transaction to be published in a block. \
--          \Use the GET /contracts/{contractId} endpoint to poll the on-chain status."
--      :> OperationId "submitContract"
--      :> PutSignedTxAPI

type GetContractAPI =
  Summary "Get contract by ID"
    :> OperationId "getContractById"
    -- :> RenameResponseSchema "GetContractResponse"
    :> UVerb
        'GET
        '[JSON]
        '[ GetContractResponse ]

type GetContractResponse = WithLink "transactions" ContractState

instance HasStatus GetContractResponse where
  type StatusOf GetContractResponse = 200

data ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: Maybe PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance NFData ContractHeader

instance HasPagination ContractHeader "contractId" where
  type RangeType ContractHeader "contractId" = ContractId
  getFieldValue _ ContractHeader{..} = contractId

data PostContractsRequest = PostContractsRequest
  { tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , version :: MarloweVersion
  , roles :: Maybe RolesConfig
  , threadTokenName :: Maybe Text
  , contract :: ContractOrSourceId
  , accounts :: Map Party Assets
  , minUTxODeposit :: Maybe Integer
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

type PostContractsResponse tx = WithLink "contract" (CreateTxEnvelope tx)

instance HasStatus (PostContractsResponse CardanoTx) where
  type StatusOf (PostContractsResponse CardanoTx) = 201

data ContractState = ContractState
  { contractId :: ContractId
  , roleTokenMintingPolicyId :: Maybe PolicyId
  , version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , initialContract :: Semantics.Contract
  , initialState :: Semantics.State
  , currentContract :: Maybe Semantics.Contract
  , state :: Maybe Semantics.State
  , utxo :: Maybe TxOutRef
  , assets :: Assets
  , txBody :: Maybe TextEnvelope
  , unclaimedPayouts :: [Payout]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

instance NFData ContractState

