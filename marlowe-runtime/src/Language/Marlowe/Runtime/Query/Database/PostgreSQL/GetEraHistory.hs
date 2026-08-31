{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetEraHistory where

import qualified Cardano.Api as C
import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.Query.Database (GetEraHistoryError (InvalidEraHistory, MissingEraHistory))

type GetEraHistory m = m (Either GetEraHistoryError C.EraHistory)

getEraHistory :: GetEraHistory T.Transaction
getEraHistory = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT node_status.value :: bytea
          FROM marlowe.node_status
          WHERE node_status.attr = 'eraHistory'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleEraHistory -> do
    bytes <- note MissingEraHistory possibleEraHistory
    bimap (const $ InvalidEraHistory bytes) id $
      C.deserialiseFromCBOR (C.proxyToAsType (Proxy :: Proxy C.EraHistory)) bytes
