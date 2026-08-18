{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Indexer.Database.PostgreSQL.CommitNodeTip where

import Data.Binary (put)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Hasql.TH (resultlessStatement)
import qualified Hasql.Transaction as H
import Language.Marlowe.Runtime.ChainSync.Api (NodeTip (..))

commitNodeTip :: NodeTip -> H.Transaction ()
commitNodeTip tip =
  H.statement
    (prepareParams tip)
    [resultlessStatement|
    INSERT INTO marlowe.node_status (attr, value)
    VALUES ('tip', $1 :: bytea)
    ON CONFLICT (attr) DO UPDATE SET value = EXCLUDED.value
  |]

type QueryParams = ByteString

prepareParams :: NodeTip -> QueryParams
prepareParams = toStrict . runPut . put
