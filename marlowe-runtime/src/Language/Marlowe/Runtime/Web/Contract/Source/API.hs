{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.Source.API (
  ContractSourcesAPI,
  ContractSourceAPI,
  ContractSourceId (..),
  PostContractSourceResponse (..),
  ContractOrSourceId (..),
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  withText,
 )
import Data.Map (Map)
import GHC.Generics (Generic)
import Marlowe.Plutus.Semantics.Types (Contract)
import qualified Marlowe.Plutus.Semantics.Types as Semantics
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject, OperationId, RenameResponseSchema)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Pipes (Producer)
import Servant (
  Description,
  FromHttpApiData,
  JSON,
  NewlineFraming,
  Proxy (..),
  QueryFlag,
  QueryParam',
  Required,
  StreamBody,
  Summary,
  ToHttpApiData,
  type (:<|>),
  type (:>)
 )
import Servant.API (FromHttpApiData (..), UVerb, StdMethod (POST, GET), HasStatus, StatusOf, Capture)

import Control.Lens ((&), (?~))
import Control.Monad ((<=<))
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import Data.OpenApi (
  HasOneOf (oneOf),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiString),
  ToParamSchema (..),
  ToSchema (..),
  declareSchemaRef,
 )
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Language.Marlowe.Runtime.Web.Adapter.ByteString (hasLength)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))


-- | /contracts/sources sub-API
type ContractSourcesAPI =
  PostContractSourcesAPI
    :<|> Capture "contractSourceId" ContractSourceId
          :> ContractSourceAPI

-- | /contracts/sources/:contractSourceId sub-API
type ContractSourceAPI =
  GetContractSourceAPI
    :<|> "adjacency"
      :> Summary "Get adjacent contract source IDs by ID"
      :> Description
          "Get the contract source IDs which are adjacent to a contract source (they appear directly in the contract source)."
      :> OperationId "getContractSourceAdjacency"
      :> GetContractSourceIdsAPI
    :<|> "closure"
      :> Summary "Get contract source closure by ID"
      :> Description
          "Get the contract source IDs which appear in the full hierarchy of a contract source (including the ID of the contract source its self)."
      :> OperationId "getContractSourceClosure"
      :> GetContractSourceIdsAPI

type PostContractSourcesAPI =
  Summary "Upload contract sources"
    :> Description
        "Upload a bundle of marlowe objects as contract sources. This API supports request body streaming, with newline \
        \framing between request bundles."
    :> OperationId "initContractSources"
    :> QueryParam' '[Required, Description "The label of the top-level contract object in the bundle(s)."] "main" Label
    :> StreamBody NewlineFraming JSON (Producer ObjectBundle IO ())
    :> UVerb
        'POST
        '[JSON]
        '[PostContractSourceResponse]

instance HasStatus Contract where
  type StatusOf Contract = 200

type GetContractSourceAPI =
  Summary "Get contract source by ID"
    :> OperationId "getContractSourceById"
    :> QueryFlag "expand"
    :> UVerb
        'GET
        '[JSON]
        '[Contract]

type GetContractSourceIdsAPI =
  RenameResponseSchema "ContractSourceIds"
    :> UVerb
        'GET
        '[JSON]
        '[ListObject ContractSourceId]

data PostContractSourceResponse = PostContractSourceResponse
  { contractSourceId :: ContractSourceId
  , intermediateIds :: Map Label ContractSourceId
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance HasStatus PostContractSourceResponse where
  type StatusOf PostContractSourceResponse = 200

newtype ContractSourceId = ContractSourceId {unContractSourceId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON) via Base16

instance FromHttpApiData ContractSourceId where
  parseUrlPiece = fmap ContractSourceId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance FromJSON ContractSourceId where
  parseJSON =
    withText "ContractSourceId" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema ContractSourceId where
  declareNamedSchema = pure . NamedSchema (Just "ContractSourceId") . toParamSchema

instance HasStatus (ListObject ContractSourceId) where
  type StatusOf (ListObject ContractSourceId) = 200

instance ToParamSchema ContractSourceId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Marlowe contract source"
      & OpenApi.pattern ?~ "^[a-fA-F0-9]{64}$"

newtype ContractOrSourceId = ContractOrSourceId (Either Semantics.Contract ContractSourceId)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ContractOrSourceId where
  parseJSON =
    fmap ContractOrSourceId . \case
      String "close" -> pure $ Left Semantics.Close
      String s -> Right <$> parseJSON (String s)
      j -> Left <$> parseJSON j

instance ToJSON ContractOrSourceId where
  toJSON = \case
    ContractOrSourceId (Left contract) -> toJSON contract
    ContractOrSourceId (Right hash) -> toJSON hash

instance ToSchema ContractOrSourceId where
  declareNamedSchema _ = do
    contractSchema <- declareSchemaRef $ Proxy @Semantics.Contract
    contractSourceIdSchema <- declareSchemaRef $ Proxy @ContractSourceId
    pure $
      NamedSchema Nothing $
        mempty
          & oneOf ?~ [contractSchema, contractSourceIdSchema]

