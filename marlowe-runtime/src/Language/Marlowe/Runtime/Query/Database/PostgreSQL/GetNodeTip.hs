{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Query.Database.PostgreSQL.GetNodeTip where

import Control.Error (note)
import Data.Bifunctor (bimap)
import Data.Binary (get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString, fromStrict)
import Data.Profunctor (rmap)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement)
import Hasql.Transaction qualified as T
import Language.Marlowe.Runtime.ChainSync.Api (NodeTip)
import Language.Marlowe.Runtime.Query.Database (GetNodeTipError (MissingNodeTip, InvalidNodeTip))

type GetNodeTip m = m (Either GetNodeTipError NodeTip)

getNodeTip :: GetNodeTip T.Transaction
getNodeTip = do
  let
    s :: Statement () (Maybe ByteString)
    s =
      [maybeStatement|
          SELECT node_status.value :: bytea
          FROM marlowe.node_status
          WHERE node_status.attr = 'tip'
          LIMIT 1
      |]
  T.statement () $ flip rmap s \possibleTip -> do
    tipBytes <- note MissingNodeTip possibleTip
    bimap (const $ InvalidNodeTip tipBytes) (\(_, _, tip) -> tip) . runGetOrFail get . fromStrict $ tipBytes




