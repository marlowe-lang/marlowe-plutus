{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitIndexerTip where

import Data.Binary (put)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api (IndexerTip (..))

commitIndexerTip :: IndexerTip -> H.Transaction ()
commitIndexerTip tip =
  H.statement
    (prepareParams tip)
    [resultlessStatement|
    INSERT INTO marlowe.indexer_status (attr, value)
    VALUES ('tip', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

type QueryParams = ByteString

prepareParams :: IndexerTip -> QueryParams
prepareParams = toStrict . runPut . put
