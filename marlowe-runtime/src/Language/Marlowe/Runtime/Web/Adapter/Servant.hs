{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Marlowe.Runtime.Web.Adapter.Servant (
  WithRuntimeStatus,
  OperationId,
  RenameResponseSchema,
  RenameSchema,
  ListObject (..),
  AddRenameSchema,
) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (
  NamedSchema (..),
  ToSchema,
  allOperations,
  declareNamedSchema,
  operationId, PathItem (..), OpenApi (..),
 )
import qualified Data.Text as T
import GHC.Base (Symbol, Alternative ((<|>)))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Servant (
  HasServer (..),
  Headers,
  IsElem,
  IsElem',
  Proxy (..),
  Stream,
  Verb,
  type (:<|>),
  type (:>), UVerb, HasStatus (StatusOf),
 )
import Servant.Client (HasClient (..))
import Servant.OpenApi (HasOpenApi (toOpenApi))
import qualified Servant.OpenApi.Internal as OpenApi
import Data.Kind (Type)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

data WithRuntimeStatus api

data OperationId (name :: Symbol)

data RenameResponseSchema (name :: Symbol)

data RenameSchema (name :: Symbol) a

instance
  {-# OVERLAPPABLE #-}
  ( ToSchema a,
    HasStatus a,
    OpenApi.AllToResponseHeader hs,
    OpenApi.AllAccept cs,
    OpenApi.OpenApiMethod method,
    HasOpenApi (UVerb method cs as)
  ) =>
  HasOpenApi (UVerb method cs (Headers hs a ': as))
  where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy (Verb method (StatusOf a) cs (Headers hs a)))
      `combineSwagger` toOpenApi (Proxy :: Proxy (UVerb method cs as))
    where
      -- workaround for https://github.com/GetShopTV/swagger2/issues/218
      combinePathItem :: PathItem -> PathItem -> PathItem
      combinePathItem s t = PathItem
        { _pathItemGet = _pathItemGet s <> _pathItemGet t
        , _pathItemPut = _pathItemPut s <> _pathItemPut t
        , _pathItemPost = _pathItemPost s <> _pathItemPost t
        , _pathItemDelete = _pathItemDelete s <> _pathItemDelete t
        , _pathItemOptions = _pathItemOptions s <> _pathItemOptions t
        , _pathItemHead = _pathItemHead s <> _pathItemHead t
        , _pathItemPatch = _pathItemPatch s <> _pathItemPatch t
        , _pathItemTrace = _pathItemTrace s <> _pathItemTrace t
        , _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
        , _pathItemSummary = _pathItemSummary s <|> _pathItemSummary t
        , _pathItemDescription = _pathItemDescription s <|> _pathItemDescription t
        , _pathItemServers = _pathItemServers s <> _pathItemServers t
        }

      combineSwagger :: OpenApi -> OpenApi -> OpenApi
      combineSwagger s t = OpenApi
        { _openApiOpenapi = _openApiOpenapi s <> _openApiOpenapi t
        , _openApiInfo = _openApiInfo s <> _openApiInfo t
        , _openApiServers = _openApiServers s <> _openApiServers t
        , _openApiPaths = InsOrdHashMap.unionWith combinePathItem (_openApiPaths s) (_openApiPaths t)
        , _openApiComponents = _openApiComponents s <> _openApiComponents t
        , _openApiSecurity = _openApiSecurity s <> _openApiSecurity t
        , _openApiTags = _openApiTags s <> _openApiTags t
        , _openApiExternalDocs = _openApiExternalDocs s <|> _openApiExternalDocs t
        }


type family MapRenameSchema name (xs :: [Type]) :: [Type] where
  MapRenameSchema name '[]       = '[]
  MapRenameSchema name (Headers hs a ': xs) =
    Headers hs (RenameSchema name a) ': MapRenameSchema name xs
  MapRenameSchema name (x ': xs) = RenameSchema name x ': MapRenameSchema name xs

type family AddRenameSchema name api where
  AddRenameSchema name (path :> api) = path :> AddRenameSchema name api
  AddRenameSchema name (a :<|> b) = AddRenameSchema name a :<|> AddRenameSchema name b
  AddRenameSchema name (Verb method cTypes status (Headers hs a)) =
    Verb method cTypes status (Headers hs (RenameSchema name a))
  AddRenameSchema name (Verb method cTypes status a) = Verb method cTypes status (RenameSchema name a)
  AddRenameSchema name (UVerb method cTypes xs) = UVerb method cTypes (MapRenameSchema name xs)
  AddRenameSchema name (Stream method status framing ct (Headers hs a)) =
    Stream method status framing ct (Headers hs (RenameSchema name a))
  AddRenameSchema name (Stream cTypes status framing ct a) = Stream cTypes status framing ct (RenameSchema name a)

instance (KnownSymbol name, ToSchema a) => ToSchema (RenameSchema name a) where
  declareNamedSchema _ = do
    NamedSchema _ schema <- declareNamedSchema $ Proxy @a
    pure $ NamedSchema (Just $ T.pack $ symbolVal $ Proxy @name) schema

instance (HasStatus a) => HasStatus (RenameSchema name a) where
  type StatusOf (RenameSchema name a) = StatusOf a

instance (HasServer sub ctx) => HasServer (OperationId name :> sub) ctx where
  type ServerT (OperationId name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (OperationId name :> api) where
  type Client m (OperationId name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi api) => HasOpenApi (OperationId name :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & allOperations . operationId ?~ T.pack (symbolVal $ Proxy @name)

instance (HasServer sub ctx) => HasServer (RenameResponseSchema name :> sub) ctx where
  type ServerT (RenameResponseSchema name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (RenameResponseSchema name :> api) where
  type Client m (RenameResponseSchema name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi (AddRenameSchema name api)) => HasOpenApi (RenameResponseSchema name :> api) where
  toOpenApi _ = toOpenApi $ Proxy @(AddRenameSchema name api)

type instance IsElem' e (WithRuntimeStatus api) = IsElem e api

-- | A wrapper for a list of objects.
newtype ListObject a = ListObject {results :: [a]}
  deriving (Eq, Show, Ord, Functor, GHC.Generics.Generic)

instance (ToJSON a) => ToJSON (ListObject a)
instance (FromJSON a) => FromJSON (ListObject a)
instance (ToSchema a) => ToSchema (ListObject a)
