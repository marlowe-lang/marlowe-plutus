{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitEraHistory where

import qualified Cardano.Api as C
import Data.ByteString (ByteString)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H

commitEraHistory :: C.EraHistory -> H.Transaction ()
commitEraHistory eraHistory =
  H.statement
    (prepareParams eraHistory)
    [resultlessStatement|
    INSERT INTO marlowe.node_status (attr, value)
    VALUES ('eraHistory', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

type QueryParams = ByteString

prepareParams :: C.EraHistory -> QueryParams
prepareParams = C.serialiseToCBOR
